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
  var acks = Map.empty[Long, (ActorRef, Replicate, Cancellable)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  var _seqCounter = 0L
  def nextSeq() = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case Replicate(key, valueOption, seq) => {
      log.info(s"Replicator - Replicate($key, $valueOption, $seq)")
      val replicareScheduler = context.system.scheduler.schedule(100.milliseconds, 150.milliseconds) {
        replica ! Snapshot(key, valueOption, seq)
      }
      acks = acks.updated(seq, (sender(), Replicate(key, valueOption, seq), replicareScheduler))

    }
    case SnapshotAck(key, seq) => {
      val request = acks(seq)
      request._3.cancel()
      request._1 ! Replicated(key, request._2.id)
  }
  }

}