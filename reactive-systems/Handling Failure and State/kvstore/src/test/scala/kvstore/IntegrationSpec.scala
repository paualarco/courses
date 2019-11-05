/**
 * Copyright (C) 2013-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package kvstore

import akka.actor.Props
import akka.testkit.TestProbe
import org.scalatest.{FunSuiteLike, Matchers}

trait IntegrationSpec
  extends FunSuiteLike
        with Matchers { this: KVStoreSuite =>

  import Arbiter._

  /*
   * Recommendation: write a test case that verifies proper function of the whole system,
   * then run that with flaky Persistence and/or unreliable communication (injected by
   * using an Arbiter variant that introduces randomly message-dropping forwarder Actors).
   */



  test("Integration-case2") {
    val arbiter = system.actorOf(Props(classOf[Arbiter]), "integration-case2-arbiter")
    val primary = system.actorOf(Replica.props(arbiter, Persistence.props(flaky = true)), "integration-case2-primary")
    val client = session(primary)
    client.get("k1") === None

    //TODO use kvstore.given.Arbiter instead of Unreliable
    val unreliableReplica = system.actorOf(Unreliable.props(Replica.props(arbiter, Persistence.props(flaky = true))), "integration-case2-unreliable-replica")
    client.setAcked("k1", "v1")
  }


  }

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

import scala.util.Random

object Unreliable {
  def props(childProps: Props) = Props(classOf[Unreliable], childProps)
}

class Unreliable(childProps: Props) extends Actor with ActorLogging {
  val child: ActorRef = context.actorOf(childProps)

  override def receive: Receive = {
    case msg if Random.nextDouble() < 0.8 =>
      log.warning("{} not forwarded to {}", msg, child)
    case msg =>
      log.info("forward {} to {}", msg, child)
      child forward msg
  }
}