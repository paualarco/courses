package kvstore

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, OneForOneStrategy, PoisonPill, Props, SupervisorStrategy, Terminated}
import kvstore.Arbiter._
import akka.pattern.{ask, pipe}

import scala.concurrent.duration._
import akka.util.Timeout

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  case class PersistenceTimeout(key: String, id: Long)

  sealed trait PersistedData
  case class PersistedSecondaryData(valueOption: Option[String], sender: ActorRef, scheduler: Cancellable) extends PersistedData
  case class PersistedPrimaryData(valueOption: Option[String],
                                  sender: ActorRef,
                                  scheduler: Cancellable,
                                  timeoutScheduler: Cancellable,
                                  globalAckTimeoutScheduler: Cancellable,
                                  isLocallyPersisted: Boolean,
                                  isGloballyPersisted: Boolean,
                                  replicatorsAcknowledgement:  Set[ActorRef]) extends PersistedData

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
  case class OperationTimeout(id: Long, sender: ActorRef)
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor with ActorLogging {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef] //replica -> replicator
  // the current set of replicators
  //var replicators = Set.empty[ActorRef]

  var pendingPersistance = Map.empty[Long, PersistedData]
  val persistee = context.actorOf(persistenceProps)
  context.watch(persistee)
  arbiter ! Arbiter.Join

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Snapshot(key, valueOption, seq) => log.info("Replica leader - SNAPSHOT")

      case Get(key, id) => sender() ! Replica.GetResult(key, kv.get(key), id)

    case Insert(key, value, id) => {
      kv = kv.updated(key, value)
      //secondaries.values.foreach(_ ! Replicator.Replicate(key, Some(value), id))
      val scheduler = context.system.scheduler.schedule(100.milliseconds, 150.milliseconds) {
        persistee ! Persist(key, Some(value), id)
      }
      log.info("Leader Replica - Sending Replicate Insert message to all replicators")
      secondaries.values foreach (_ ! Replicate(key, Some(value), id))

      val timeoutScheduler = context.system.scheduler.scheduleOnce(1.second, self, OperationTimeout(id, sender()))
      val globalAckTimeoutScheduler = context.system.scheduler.scheduleOnce(1.second, self, OperationTimeout(id, sender()))

      log.info(s"Leader Replica - Secondaries: ${secondaries.mkString(",")}, size ${secondaries.size}, isEmpty: ${secondaries.isEmpty}")

      val persistedPrimaryData = Replica.PersistedPrimaryData(
        Some(value),
        sender(),
        scheduler,
        timeoutScheduler,
        globalAckTimeoutScheduler,
        false,
        secondaries.isEmpty,
        Set.empty[ActorRef])

      pendingPersistance = pendingPersistance.updated(id, persistedPrimaryData)
    }

    case OperationTimeout(id, client) => {
      log.error(s"Replica OperationTImeout! The operation failed, returning OperationFailed($id)")
      client ! OperationFailed(id)
    }

    case Remove(key, id) => {
      log.info("Leader Replica - Sending Replicate Remove message to all replicators")
      kv = kv - (key)
      secondaries.values foreach (_ ! Replicate(key, None, id))

      val scheduler = context.system.scheduler.schedule(100.milliseconds, 150.milliseconds) {persistee ! Persist(key, None, id)}
      val timeoutScheduler = context.system.scheduler.scheduleOnce(1.second, self, OperationTimeout(id, sender()))
      val globalAckTimeoutScheduler = context.system.scheduler.scheduleOnce(1.second, self, OperationTimeout(id, sender()))

      pendingPersistance = pendingPersistance.updated(id,
        Replica.PersistedPrimaryData(None,
          sender(),
          scheduler,
          timeoutScheduler,
          globalAckTimeoutScheduler,
          false,
          secondaries.isEmpty,
          Set.empty[ActorRef]))
    }

    case Persisted(key, id) => {
      log.info(s"Primary - Persisted($key, $id)")
      pendingPersistance get id match {
        case Some(persistedData: PersistedPrimaryData) => {
          //pendingPersistance = pendingPersistance - id
          persistedData.scheduler.cancel()
          persistedData.timeoutScheduler.cancel()
          val updatedPersistedData = persistedData.copy(isLocallyPersisted = true)
          pendingPersistance = pendingPersistance.updated(id, updatedPersistedData)
          if(updatedPersistedData.isLocallyPersisted && updatedPersistedData.isGloballyPersisted) persistedData.sender ! OperationAck(id)

        }

      }


    }

    case Replicas(currentReplicas) => {
      val newReplicas = currentReplicas -- secondaries.keys.toSet[ActorRef] - self
      val droppedReplicas = secondaries.keys.toSet[ActorRef] -- currentReplicas - self

      log.info(s"Replicas - Existing replicas: ${secondaries.keys.mkString(",")}")
      log.info(s"Replicas - Current replicas: ${currentReplicas.mkString(",")}")
      log.info(s"Replicas - NewReplicas: ${newReplicas.mkString(",")}, size: ${newReplicas.size}, isEmpty: ${newReplicas.isEmpty}")
      log.info(s"Replicas - Dropped replicas: ${droppedReplicas.mkString(",")}, size: ${droppedReplicas.size}, isEmpty: ${droppedReplicas.isEmpty}")

      val newSecondaries = newReplicas.map { replica =>
        (replica, context.actorOf(Replicator.props(replica)))
      }
      val droppedSecondaries = secondaries.filterKeys { replica =>
        droppedReplicas.contains(replica)
      }
      val newReplicators = newSecondaries.map { _._2 }
      val droppedReplicators = droppedReplicas.map { secondaries(_) }

      secondaries = secondaries -- droppedSecondaries.keySet ++ newSecondaries

      for {
        replicator <- newReplicators
        (key, value) <- kv
      } replicator ! Replicate(key, Some(value), 0L)

      droppedReplicators.foreach { context.stop(_) }
    }

    case Replicator.Replicated(key, id) => {
      log.info("Replica Replicated received!")

      pendingPersistance get id match {
        case Some(persistedData: PersistedPrimaryData) => {
          val updatedReplicatorsAcknowledgement = persistedData.replicatorsAcknowledgement + sender()

          val isGloballyAcknowledged = updatedReplicatorsAcknowledgement.size >= secondaries.size
          //pendingPersistance = pendingPersistance - id
          if(isGloballyAcknowledged) {
            persistedData.globalAckTimeoutScheduler.cancel()
          }
          val updatedPersistedData = persistedData.copy(replicatorsAcknowledgement = updatedReplicatorsAcknowledgement, isGloballyPersisted = isGloballyAcknowledged)
          pendingPersistance = pendingPersistance.updated(id, updatedPersistedData)
          if(updatedPersistedData.isLocallyPersisted && updatedPersistedData.isGloballyPersisted) persistedData.sender ! OperationAck(id)
        }
      }
    }

    case Terminated(persistee: ActorRef) => {
      log.error("-----Terminated received!! OperationFailed returned")
      pendingPersistance.map{
        case (id, persist: PersistedSecondaryData) => {
          persist.sender ! OperationFailed(id)
        }
      }
    }
  }

  var expectedSeq: Long = 0L


  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Get(key, id) => sender() ! Replica.GetResult(key, kv.get(key), id)

    case Snapshot(key, valueOption, seq) => {
      log.info(s"Secondary - Snapshot($key, $valueOption, $seq) received, expectedSeq:$expectedSeq")
      if(seq == expectedSeq) {
        //context.system.scheduler.scheduleOnce(100.milliseconds, self, PersistenceTimeout(key, seq) )
        val scheduler = context.system.scheduler.schedule(100.milliseconds, 150.milliseconds) {
          persistee ! Persist(key, valueOption, seq)
        }
        pendingPersistance = pendingPersistance.updated(seq, Replica.PersistedSecondaryData(valueOption, sender(), scheduler))
        valueOption match {
          case Some(value) =>
            log.info("Secondary Snapshot - Insert operation")
            kv = kv.updated(key, value)
          case None =>
            log.info("Secondary Snapshot - Remove operation")
            kv = kv - (key)
        }
      } else if (seq < expectedSeq) {
        log.info(s"Secondary - Seq:$seq is lower than expectedSeq")
        sender() ! SnapshotAck(key, seq)

      }
    }

    case Persisted(key, seq) => {
      log.info(s"Replica - Persisted($key, $seq)")
      expectedSeq += 1L
      pendingPersistance get seq match {
        case Some(persist: PersistedSecondaryData) => {
          pendingPersistance = pendingPersistance - seq
          persist.sender ! SnapshotAck(key, seq)
          persist.scheduler.cancel()
        }
      }
    }
  }
}

