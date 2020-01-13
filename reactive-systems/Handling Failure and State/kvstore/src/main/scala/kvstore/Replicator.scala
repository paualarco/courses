package kvstore

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props}

import scala.concurrent.duration._

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor with ActorLogging {
  import Replicator._
  import context.dispatcher
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, ReplicateStatus]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  var _seqCounter = 0L
  def nextSeq() = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  case class ReplicateStatus(id: Long,
                             key: String,
                             valueOption: Option[String],
                             primary: ActorRef,
                             timeout: Cancellable)
  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case Replicate(key, valueOption, id) => {
      log.info(s"Replicator - Replicate($key, $valueOption, $id)")
      val seq = nextSeq()
      val replicateStatus = ReplicateStatus (
        id = id,
        key = key,
        valueOption = valueOption,
        primary = sender,
        timeout =
          context.system.scheduler.schedule(100.milliseconds, 150.milliseconds) {
            replica ! Snapshot(key, valueOption, seq)
          }
      )
      replica ! Snapshot(key, valueOption, seq)
      val replicareScheduler =
      acks = acks.updated(seq, replicateStatus)
    }

    case SnapshotAck(key, seq) => {
      val request = acks(seq)
      request.timeout.cancel()
      log.info(s"Replicator - received SnapshotAck($key, $seq)")
      request.primary ! Replicated(key, request.id)
  }
  }

}
