package services.crunch

import controllers.ArrivalGenerator
import drt.shared.CrunchApi.PortState
import drt.shared.FlightsApi.Flights
import drt.shared.PaxTypesAndQueues._
import drt.shared.SplitRatiosNs.{SplitRatio, SplitRatios, SplitSources}
import drt.shared._
import server.feeds.ArrivalsFeedSuccess
import services.SDate
import services.graphstages.Crunch._

import scala.collection.immutable.List
import scala.concurrent.duration._

class CrunchQueueAndTerminalValidationSpec extends CrunchTestLike {
  sequential
  isolated

  "Queue validation " >> {
    "Given a flight with transfers " +
      "When I ask for a crunch " +
      "Then I should see only the non-transfer queue" >> {
      val scheduled00 = "2017-01-01T00:00Z"
      val scheduled = "2017-01-01T00:00Z"

      val flights = Flights(Seq(
        ArrivalGenerator.apiFlight(flightId = Option(1), schDt = scheduled00, iata = "BA0001", terminal = "T1", actPax = Option(15))
      ))

      val fiveMinutes = 600d / 60

      val crunch = runCrunchGraph(
        now = () => SDate(scheduled),
        airportConfig = airportConfig.copy(
          defaultPaxSplits = SplitRatios(
            SplitSources.TerminalAverage,
            SplitRatio(eeaMachineReadableToDesk, 1),
            SplitRatio(PaxTypeAndQueue(PaxTypes.EeaMachineReadable, Queues.Transfer), 1)
          ),
          defaultProcessingTimes = Map("T1" -> Map(eeaMachineReadableToDesk -> fiveMinutes))
        ))

      offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(flights))

      val expected = Set(Queues.EeaDesk)

      crunch.liveTestProbe.fishForMessage(10 seconds) {
        case ps: PortState =>
          val resultSummary = paxLoadsFromPortState(ps, 1).flatMap(_._2.keys).toSet
          resultSummary == expected
      }

      true
    }
  }

  "Given two flights, one with an invalid terminal " +
    "When I ask for a crunch " +
    "I should only see crunch results for the flight with a valid terminal" >> {

    val scheduled = "2017-01-01T00:00Z"

    val flights = Flights(Seq(
      ArrivalGenerator.apiFlight(flightId = Option(1), schDt = scheduled, iata = "BA0001", terminal = "T1", actPax = Option(15)),
      ArrivalGenerator.apiFlight(flightId = Option(2), schDt = scheduled, iata = "FR8819", terminal = "XXX", actPax = Option(10))
    ))

    val fiveMinutes = 600d / 60

    val crunch = runCrunchGraph(
      now = () => SDate(scheduled),
      airportConfig = airportConfig.copy(
        defaultProcessingTimes = Map("T1" -> Map(eeaMachineReadableToDesk -> fiveMinutes))
      ))

    offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(flights))

    val expected = Map("T1" -> Map(Queues.EeaDesk -> List(15.0)))

    crunch.liveTestProbe.fishForMessage(10 seconds) {
      case ps: PortState =>
        val resultSummary = paxLoadsFromPortState(ps, 1, SDate(scheduled))
        resultSummary == expected
    }

    true
  }
}
