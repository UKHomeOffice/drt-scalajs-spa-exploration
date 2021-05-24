package test.feeds.test

import actors.acking.AckingReceiver.Ack
import akka.NotUsed
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Cancellable}
import akka.pattern.ask
import akka.stream.scaladsl.Source
import akka.util.Timeout
import drt.shared.FlightsApi.Flights
import drt.shared.api.Arrival
import org.slf4j.{Logger, LoggerFactory}
import server.feeds.{ArrivalsFeedResponse, ArrivalsFeedSuccess}
import services.SDate
import test.TestActors.ResetData

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps

object TestFixtureFeed {

  val log: Logger = LoggerFactory.getLogger(getClass)

  def apply(actorSystem: ActorSystem, testArrivalActor: ActorRef): Source[ArrivalsFeedResponse, Cancellable] = {

    log.info(s"About to create test Arrival")

    implicit val timeout: Timeout = Timeout(1 seconds)

    val pollFrequency = 2 seconds
    val initialDelayImmediately: FiniteDuration = 1 milliseconds
    val tickingSource = Source
      .tick(initialDelayImmediately, pollFrequency, NotUsed)
      .mapAsync(1) { _ =>
        testArrivalActor
          .ask(GetArrivals)
          .map { case Arrivals(arrivals) => arrivals }
          .recover { case _ => List() }
      }
      .collect {
        case arrivals if arrivals.nonEmpty => ArrivalsFeedSuccess(Flights(arrivals), SDate.now())
      }

    tickingSource
  }
}

case object GetArrivals

case class Arrivals(arrivals: List[Arrival])

class TestArrivalsActor extends Actor with ActorLogging {

  var testArrivals: Option[List[Arrival]] = None

  override def receive: PartialFunction[Any, Unit] = {
    case a: Arrival =>
      log.info(s"TEST: Appending test arrival $a")

      testArrivals = testArrivals match {
        case None => Option(List(a))
        case Some(existing) => Option(existing :+ a)
      }

      sender() ! Ack

    case GetArrivals =>
      val toSend = Arrivals(testArrivals.getOrElse(List()))
      if (toSend.arrivals.nonEmpty) log.info(s"Sending test arrivals: ${toSend.arrivals.size}")
      sender() ! toSend
      testArrivals = None

    case ResetData =>
      testArrivals = None
      sender() ! Ack
  }
}
