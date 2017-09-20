package services.crunch

import akka.pattern.AskableActorRef
import akka.stream.scaladsl.Source
import akka.testkit.TestProbe
import controllers.ArrivalGenerator
import drt.shared.FlightsApi.Flights
import drt.shared.PaxTypesAndQueues._
import drt.shared.SplitRatiosNs.{SplitRatio, SplitRatios, SplitSources}
import drt.shared._
import passengersplits.parsing.VoyageManifestParser.{VoyageManifest, VoyageManifests}
import services.Crunch._
import services.SDate

import scala.collection.immutable.List

class CrunchQueueAndTerminalValidationSpec extends CrunchTestLike {
  "Queue validation " >> {
    "Given a flight with transfers " +
      "When I ask for a crunch " +
      "Then I should see only the non-transfer queue" >> {
      val scheduled00 = "2017-01-01T00:00Z"
      val scheduled = "2017-01-01T00:00Z"

      val flights = List(Flights(List(
        ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled00, iata = "BA0001", terminal = "T1", actPax = 15)
      )))

      val fiveMinutes = 600d / 60
      val procTimes: Map[PaxTypeAndQueue, Double] = Map(eeaMachineReadableToDesk -> fiveMinutes)

      val testProbe = TestProbe()
      val runnableGraphDispatcher: (Source[Flights, _], Source[VoyageManifests, _]) => AskableActorRef =
        runCrunchGraph(
          procTimes = procTimes,
          testProbe = testProbe,
          crunchStartDateProvider = () => getLocalLastMidnight(SDate(scheduled)).millisSinceEpoch,
          portSplits = SplitRatios(
            SplitSources.TerminalAverage,
            SplitRatio(eeaMachineReadableToDesk, 1),
            SplitRatio(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.Transfer), 1)
          )
        )

      runnableGraphDispatcher(Source(flights), Source(List()))

      val result = testProbe.expectMsgAnyClassOf(classOf[CrunchState])
      val resultSummary = paxLoadsFromCrunchState(result, 1).flatMap(_._2.keys)

      val expected = Set(Queues.EeaDesk)

      resultSummary === expected
    }
  }

  "Given two flights, one with an invalid terminal " +
    "When I ask for a crunch " +
    "I should only see crunch results for the flight with a valid terminal" >> {

    val scheduled = "2017-01-01T00:00Z"

    val flights = List(Flights(List(
      ArrivalGenerator.apiFlight(flightId = 1, schDt = scheduled, iata = "BA0001", terminal = "T1", actPax = 15),
      ArrivalGenerator.apiFlight(flightId = 2, schDt = scheduled, iata = "FR8819", terminal = "XXX", actPax = 10)
    )))

    val fiveMinutes = 600d / 60
    val procTimes: Map[PaxTypeAndQueue, Double] = Map(eeaMachineReadableToDesk -> fiveMinutes)

    val testProbe = TestProbe()
    val runnableGraphDispatcher: (Source[Flights, _], Source[VoyageManifests, _]) => AskableActorRef =
      runCrunchGraph(procTimes = procTimes,
        testProbe = testProbe,
        crunchStartDateProvider = () => getLocalLastMidnight(SDate(scheduled)).millisSinceEpoch
      )

    runnableGraphDispatcher(Source(flights), Source(List()))

    val result = testProbe.expectMsgAnyClassOf(classOf[CrunchState])
    val resultSummary = paxLoadsFromCrunchState(result, 30)

    val expected = Map(
      "T1" -> Map(Queues.EeaDesk -> Seq(
        15.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)))

    resultSummary === expected
  }
}
