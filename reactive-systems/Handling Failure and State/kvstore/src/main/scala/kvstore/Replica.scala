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
  case class PersistedData(valueOption: Option[String], sender: ActorRef, scheduler: Cancellable)
  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
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
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  var pendingPersistance = Map.empty[Long, PersistedData]
  val persistee = context.actorOf(persistenceProps)
  arbiter ! Arbiter.Join

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Get(key, id) => sender() ! Replica.GetResult(key, kv.get(key), id)
    case Insert(key, value, id) => {
      kv = kv.updated(key, value)
      sender() ! OperationAck(id)
    }
    case Remove(key, id) => {
      kv = kv - (key)
      sender() ! OperationAck(id)
    }
    case Replicas(replicas) => {
      val newReplica = {
        val diff = secondaries.keys.toSet[ActorRef] -- replicas
        if(diff.size > 1) log.error(s"Replica Leader - Replicas received from leader not matching with secondaries, diff size is: ${diff.size}")
        diff.toSeq(0)
      }
      val newReplicator = context.actorOf(Replicator.props(newReplica))
      secondaries = secondaries + (newReplica -> newReplicator)
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
        pendingPersistance = pendingPersistance.updated(seq, Replica.PersistedData(valueOption, sender(), scheduler))

      } else if (seq < expectedSeq) {
        log.info(s"Secondary - Seq:$seq is lower than expectedSeq")
        sender() ! SnapshotAck(key, seq)

      }
    }
    case Persisted(key, seq) => {
      log.info(s"Replica - Persisted($key, $seq)")
      expectedSeq += 1L
      pendingPersistance get seq match {
        case Some(persist) => {
          pendingPersistance = pendingPersistance - seq
          persist.sender ! SnapshotAck(key, seq)
          persist.scheduler.cancel()
          persist.valueOption match {
            case Some(value) => {
              log.info("Secondary - Insert operation")
              kv = kv.updated(key, persist.valueOption.get)
            }
            case None => {
              log.info("Secondary - Remove operation")
              kv = kv - key
              context.become(replica)
            }
          }
        }
      }
    }
  }
}

