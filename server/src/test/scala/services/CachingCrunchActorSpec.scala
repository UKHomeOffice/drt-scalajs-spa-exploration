package services

import actors.{CachableActorQuery, CachingCrunchReadActor}
import akka.actor._
import akka.pattern.ask
import controllers.GetTerminalCrunch
import drt.shared.SDateLike
import drt.shared.Terminals.T1
import services.crunch.CrunchTestLike

import scala.concurrent.Await
import scala.concurrent.duration._

case object WasCalled

class TestActorProbe(pointInTime: SDateLike, incrementer: () => Unit) extends Actor {
  def receive: Receive = {
    case _: GetTerminalCrunch =>
      incrementer()
      sender() ! s"Terminal Crunch Results for ${pointInTime.toISOString()}"
  }
}

class CachingCrunchActorSpec extends CrunchTestLike {
  isolated
  sequential

  val cacheActorRef: ActorRef = system.actorOf(Props(classOf[CachingCrunchReadActor]), name = "crunch-cache-actor")
  "Should pass a message onto the crunch actor and return the response" >> {

    def inc(): Unit = {}
    val query = CachableActorQuery(Props(new TestActorProbe(SDate("2017-06-01T20:00:00Z"), inc _)), GetTerminalCrunch(T1))
    val resultFuture = cacheActorRef.ask(query)

    val result = Await.result(resultFuture, 1 second)
    val expected = "Terminal Crunch Results for 2017-06-01T20:00:00Z"

    result === expected
  }

  "When the same query is sent twice" >> {
    var called = 0
    def inc() = {
      called +=1
    }

    val query = CachableActorQuery(Props(new TestActorProbe(SDate("2017-06-01T20:00:00Z"), inc _)), GetTerminalCrunch(T1))

    val resF2 = cacheActorRef.ask(query)

    val res1 = Await.result(resF2, 1 second)
    val res2 = Await.result(resF2, 1 second)

    "The first result should equal the second" >> {
      res1 === res2
    }
    "The underlying actor should be called only once" >> {
      called === 1
    }
  }

  "When two different queries are sent" >> {
    var called = 0
    def inc() = {
      called +=1
    }

    val query1 = CachableActorQuery(Props(new TestActorProbe(SDate("2017-06-01T20:00:00Z"), inc _)), GetTerminalCrunch(T1))
    val resF1 = cacheActorRef.ask(query1)
    val res1 = Await.result(resF1, 1 second)

    val query2 = CachableActorQuery(Props(new TestActorProbe(SDate("2017-06-01T20:05:00Z"), inc _)), GetTerminalCrunch(T1))
    val resF2 = cacheActorRef.ask(query2)
    val res2 = Await.result(resF2, 1 second)

    "The first result should not equal the second" >> {
      res1 !== res2
    }
    "The underlying actor should be called twice" >> {
      called === 2
    }
  }
}
