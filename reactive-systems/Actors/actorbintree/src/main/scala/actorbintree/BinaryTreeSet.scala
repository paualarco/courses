/**
  * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
  */
package actorbintree

import akka.actor._
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import com.typesafe.config.Config

import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef

    def id: Int

    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }



  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection */
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}

  class BinaryTreeSet extends Actor with ActorLogging {

  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]
  var jobsLookUp = Map.empty[Int, ActorRef]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case op: Operation => {
      log.info(s"Normal - Operation received with id:${op.id}, elem:${op.elem}")
      root ! op
    }

    case GC => {
      log.info("Normal - GC!")
      val newRoot = context.actorOf(props(0, true))
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(sender(), newRoot))
    }
  }


  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(requester: ActorRef, newRoot: ActorRef): Receive = {
    case CopyFinished => {
      log.info("GarbageCollecting - ¡CopyFinished! So starting Migration...")
      root = newRoot
      requester ! CopyFinished
      if(pendingQueue.isEmpty){
        log.info("GarbageCollecting1 - Empty queue, so going back to normal state...")
        context.become(normal)
      }
      else {
        log.info("GarbageCollecting1 - Not empty queue, so dequeueing...")
        val (nextOp, nextQueue) = pendingQueue.dequeue
        log.debug(s"Gargage Collector to Dequeueing -> Head: ${nextOp}")
        root ! changeToSelfRequester(nextOp)
        pendingQueue = nextQueue
        context.become(dequeueing())
      }
    }
    case op: Operation => {
      log.info(s"Garbage collecting - Operation while Garbage Collecting, queue size ${pendingQueue.size}")
      jobsLookUp = jobsLookUp + (op.id -> op.requester)
      pendingQueue = pendingQueue.enqueue(op)
    }
    case GC => {
      log.info("-Garbage Collecting - GC while GarbageCollecting")
    }
  }

  def changeToSelfRequester(op: Operation): Operation = {
    op match {
      case Contains(_,_,_) => Contains(self, op.id, op.elem)
      case Insert(_,_,_) => Insert(self, op.id, op.elem)
      case Remove(_,_,_) => Remove(self, op.id, op.elem)
    }
  }

  def dequeueing(): Receive = {

    case containsResult: OperationReply => {
      log.info(s"Dequeueing - OperationReply returned back to the requester, pendingQueueSize${pendingQueue.size}")
      val requester = jobsLookUp(containsResult.id)
      log.info(s"Lookup table match ${containsResult.id}: ${jobsLookUp.mkString(", ")}")
      requester ! containsResult
      jobsLookUp = jobsLookUp - containsResult.id
      if (!pendingQueue.isEmpty) {
        val (nextOp, nextQueue) = pendingQueue.dequeue
        pendingQueue = nextQueue
        log.info(s"Dequeueing - Sending new request to root")
        root ! changeToSelfRequester(nextOp)
        dequeueing()
      } else context.become(normal)
    }

    case op: Operation => {
      log.info(s"Dequeueing - Operation while Dequeueing")
      if(!pendingQueue.isEmpty) {
        jobsLookUp = jobsLookUp + (op.id -> op.requester)
        pendingQueue = pendingQueue.enqueue(op)
        val (nextOp, nextQueue) = pendingQueue.dequeue
        pendingQueue = nextQueue
        root ! changeToSelfRequester(nextOp)
        dequeueing()
      } else {
        root ! op
        context.become(normal)
      }
    }

    case GC => {
      log.info("Dequeueing - GC!")
     /* newRoot = context.actorOf(props(0, true))
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(sender(), newRoot))*/
    }
  }

}

object BinaryTreeNode {

  trait Position

  case object Left extends Position

  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)

  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor with ActorLogging {

  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved
  var nodesCopied = Set.empty[ActorRef]
  // optional

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  def receive: Receive = {
    case Insert(requester, id, elemToInsert) => {
      log.info(s"Node $elem elemReceive - Insert(id: $id, elementToInsert: $elemToInsert)")
      def insert(position: Position): Unit = {
        subtrees = subtrees + (position -> (context.actorOf(props(elemToInsert, false))))
        requester ! OperationFinished(id)
      }
      if(elemToInsert == elem) {
        if (removed) removed = false
        requester ! OperationFinished(id)
      }
      else {
        if (subtrees.size == (0)) {
          if (elemToInsert < elem) insert(Left) else insert(Right)
        }
        else {
          if (elemToInsert < elem) {
            if (subtrees.contains(Left)) subtrees(Left) ! Insert(requester, id, elemToInsert)
            else insert(Left)
          }
          else {
            if (subtrees.contains(Right)) subtrees(Right) ! Insert(requester, id, elemToInsert)
            else insert(Right)
          }
        }
      }
    }

    case Contains(requester, id, elemToLookFor) => {
      log.info(s"Node $elem Receive - Contains(id: $id, elemToLookFor: $elemToLookFor), on node with elem: $elem")
      if (elemToLookFor == elem && !removed) requester ! ContainsResult(id, true)
      else {
        if (subtrees.isEmpty) requester ! ContainsResult(id, false)
        else {
          if (elemToLookFor < elem) {
            if (subtrees.contains(Left)) subtrees(Left) ! Contains(requester, id, elemToLookFor) else requester ! ContainsResult(id, false)
          }
          else if (subtrees contains Right) subtrees(Right) ! Contains(requester, id, elemToLookFor) else requester ! ContainsResult(id, false)
        }
      }
    }

    case Remove(requester, id, elemToRemove) => {
      log.info(s"Node $elem Receive - Remove (id:$id, elemToRemove:$elemToRemove)")
      if (elemToRemove == elem) {
        //log.info(s"Removed element: $elem ${if (removed) ", which was already removed" else ""}")
        requester ! OperationFinished(id)
        removed = true
      }
      else if (elemToRemove < elem) {
        if (subtrees contains Left) {
          subtrees(Left) ! Remove(requester, id, elemToRemove)
        }
      }
      else if (subtrees contains Right) {
        subtrees(Right) ! Remove(requester, id, elemToRemove)
      }
      else {
        requester ! OperationFinished(id)
      }
    }

    case CopyTo(newNode) => {
      log.info(s"Node $elem Receive -CopyTo($newNode)")
      var expected = subtrees.values.toSet
      if (!removed) {
      newNode ! Insert(self, elem, elem)
      expected = expected.+(self)
      }
      subtrees.values.foreach( _ ! CopyTo(newNode))
      context.become(copying(expected, newNode))
    }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], newNode: ActorRef): Receive = {

    case CopyFinished => {
      log.info(s"Node $elem Copying - CopyFinished at node with expected: ${expected.size} nodesCopied: ${nodesCopied.size}")
      nodesCopied += sender()
      if (expected == nodesCopied) context.parent ! CopyFinished
      }
  /*Actors with elements 4 and 6 finish, and sends CopyFinished to the parent, 5,
  in that case they arrive at the same time, in which the expected value is the same 2 -1 =1
  in both cases it won't empty the list of actors, and therefore the process would not endl
   */


    case OperationFinished(_) => {
      nodesCopied += self
      log.info(s"OperationFinished received at node with elem: $elem with expected: ${expected.size} nodesCopied: ${nodesCopied.size}")
      if (expected == nodesCopied) context.parent ! CopyFinished
    }

  }
}
