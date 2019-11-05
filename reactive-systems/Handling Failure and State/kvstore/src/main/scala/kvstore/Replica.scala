package kvstore

import akka.actor
import akka.actor.SupervisorStrategy.{Escalate, Restart}
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
  case class PersistedPrimaryData(key: String,
                                  valueOption: Option[String],
                                  sender: ActorRef,
                                  scheduler: Cancellable,
                                  timeoutScheduler: Cancellable,
                                  globalAckTimeoutScheduler: Cancellable,
                                  isLocallyPersisted: Boolean,
                                  //isGloballyPersisted: Boolean,
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
  var persistee = context.actorOf(persistenceProps)
  context.watch(persistee)
  arbiter ! Arbiter.Join

  override val supervisorStrategy: OneForOneStrategy = OneForOneStrategy() {
    case _: PersistenceException => Restart
    case t =>
      super.supervisorStrategy.decider.applyOrElse(t, (_: Any) => Escalate)
  }

  override def preStart(): Unit = {
    log.debug("join the replica set")
    arbiter ! Join
    log.debug("start persistence service")
    persistee = context.actorOf(persistenceProps, "persistence")
  }

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Snapshot(key, valueOption, seq) => log.info("Primary - SNAPSHOT")

    case Get(key, id) => sender() ! Replica.GetResult(key, kv.get(key), id)

    case Insert(key, value, id) => {
      log.info(s"Primary Insert - Received Insert($key, $value, $id)")

      kv = kv.updated(key, value)
      //secondaries.values.foreach(_ ! Replicator.Replicate(key, Some(value), id))
      val scheduler = context.system.scheduler.schedule(100.milliseconds, 150.milliseconds) {
        persistee ! Persist(key, Some(value), id)
      }
      log.info("Primary Insert - Sending Replicate message to all replicators")
      secondaries.values foreach (_ ! Replicate(key, Some(value), id))

      val timeoutScheduler = context.system.scheduler.scheduleOnce(1.second, self, OperationTimeout(id, sender()))
      val globalAckTimeoutScheduler = context.system.scheduler.scheduleOnce(1.second, self, OperationTimeout(id, sender()))

      log.info(s"Primary - Insert message, info : Secondaries: ${secondaries.mkString(",")}, size ${secondaries.size}, isEmpty: ${secondaries.isEmpty}")

      val persistedPrimaryData = Replica.PersistedPrimaryData(
        key,
        Some(value),
        sender(),
        scheduler,
        timeoutScheduler,
        globalAckTimeoutScheduler,
        false,
        //secondaries.isEmpty,
        Set.empty[ActorRef])

      pendingPersistance = pendingPersistance.updated(id, persistedPrimaryData)
    }

    case OperationTimeout(id, client) => {
      log.error(s"Primary  OperationTImeout! The operation failed, returning OperationFailed($id)")
      client ! OperationFailed(id)
      pendingPersistance = pendingPersistance - id
    }

    case Remove(key, id) => {
      log.info("Primary - Sending Replicate Remove message to all replicators")
      kv = kv - (key)
      secondaries.values foreach (_ ! Replicate(key, None, id))

      val scheduler = context.system.scheduler.schedule(100.milliseconds, 150.milliseconds) {persistee ! Persist(key, None, id)}
      val timeoutScheduler = context.system.scheduler.scheduleOnce(1.second, self, OperationTimeout(id, sender()))
      val globalAckTimeoutScheduler = context.system.scheduler.scheduleOnce(1.second, self, OperationTimeout(id, sender()))

      pendingPersistance = pendingPersistance.updated(id,
        Replica.PersistedPrimaryData(key,
          None,
          sender(),
          scheduler,
          timeoutScheduler,
          globalAckTimeoutScheduler,
          false,
          Set.empty[actor.ActorRef]))
    }

    case Persisted(key, id) => {
      log.info(s"Primary - Persisted($key, $id)")
      log.info(s"Primary Persisted - pendingPersistance: ${pendingPersistance.mkString(",")}!")

      pendingPersistance get id match {
        case Some(persistedData: PersistedPrimaryData) => {
          //pendingPersistance = pendingPersistance - id
          persistedData.scheduler.cancel()
          persistedData.timeoutScheduler.cancel()
          val updatedPersistedData = persistedData.copy(isLocallyPersisted = true)
          pendingPersistance = pendingPersistance.updated(id, updatedPersistedData)
          if(updatedPersistedData.isLocallyPersisted &&  (secondaries.size == updatedPersistedData.replicatorsAcknowledgement.size)) {
            log.info(s" Primary Replica - Sending OperationAck(${id}) - ${updatedPersistedData}")
            persistedData.globalAckTimeoutScheduler.cancel()
            persistedData.sender ! OperationAck(id)
            pendingPersistance = pendingPersistance - id
          }

        }

      }


    }

    case Replicas(currentReplicas) => {
      val newReplicas = currentReplicas -- secondaries.keys.toSet[ActorRef] - self
      val droppedReplicas = secondaries.keys.toSet[ActorRef] -- currentReplicas - self

      log.info(s"Primary - Existing replicas: ${secondaries.keys.mkString(",")}")
      log.info(s"Primary - Current replicas: ${currentReplicas.mkString(",")}")
      log.info(s"Primary - NewReplicas: ${newReplicas.mkString(",")}, size: ${newReplicas.size}, isEmpty: ${newReplicas.isEmpty}")
      log.info(s"Primary - Dropped replicas: ${droppedReplicas.mkString(",")}, size: ${droppedReplicas.size}, isEmpty: ${droppedReplicas.isEmpty}")
      log.info(s"Primary Replicas - pendingPersistance: ${pendingPersistance.mkString(",")}!")

      val newSecondaries = newReplicas.map { replica =>
        (replica, context.actorOf(Replicator.props(replica)))
      }
      val droppedSecondaries = secondaries.filterKeys { replica =>
        droppedReplicas.contains(replica)
      }
      val newReplicators = newSecondaries.map { _._2 }
      val droppedReplicators = droppedReplicas.map { secondaries(_) }

      secondaries = secondaries -- droppedSecondaries.keySet ++ newSecondaries

      newReplicators foreach {
        replicator =>
         kv foreach {
           case (key, value) => {
             replicator ! Replicate(key, Some(value), 0L)
           }
         }
          pendingPersistance foreach  {
            case (id, persistedData: PersistedPrimaryData) =>
              log.info(s"Primary Replicas - persisting pending persistance id: $id")

              replicator ! Replicate(persistedData.key, persistedData.valueOption, id)
          }
      }

      droppedReplicators.foreach { context.stop(_) }
    }

    case Replicator.Replicated(key, id) => {
      log.info(s"Primary Replicated received! - id: $id")
      log.info(s"Primary Replicated - pendingPersistance: ${pendingPersistance.mkString(",")}!")

      pendingPersistance get id match {
        case Some(persistedData: PersistedPrimaryData) => {
          val updatedReplicatorsAcknowledgement = persistedData.replicatorsAcknowledgement + sender()

          val isGloballyAcknowledged = updatedReplicatorsAcknowledgement.isEmpty
          //pendingPersistance = pendingPersistance - id
          if(isGloballyAcknowledged) {
            persistedData.globalAckTimeoutScheduler.cancel()
          }
          val updatedPersistedData = persistedData.copy(replicatorsAcknowledgement = updatedReplicatorsAcknowledgement) //, isGloballyPersisted = isGloballyAcknowledged)
          pendingPersistance = pendingPersistance.updated(id, updatedPersistedData)
          if(updatedPersistedData.isLocallyPersisted && (secondaries.size == updatedPersistedData.replicatorsAcknowledgement.size)) {
            log.info(s"Replica - Sending OperationAck(${id}) - $persistedData")
            persistedData.sender ! OperationAck(id)
            pendingPersistance = pendingPersistance - id
          }
        }
        case None => {
          log.info("Replica -  Not replicated event received in pendingPersistance")
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
      pendingPersistance get seq match {
        case Some(persist: PersistedSecondaryData) => {
          pendingPersistance = pendingPersistance - seq
          persist.sender ! SnapshotAck(key, seq)
          persist.scheduler.cancel()
          expectedSeq += 1L
        }
        case None =>
          log.info(s"Replica - PendingPersistance for ${seq} not found")
      }
    }
  }
}

