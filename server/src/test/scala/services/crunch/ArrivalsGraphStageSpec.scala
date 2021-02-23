package services.crunch

import actors.PartitionedPortStateActor.GetFlights
import actors.queues.FlightsRouterActor.runAndCombine
import akka.NotUsed
import akka.pattern.ask
import akka.stream.scaladsl.Source
import controllers.ArrivalGenerator
import controllers.ArrivalGenerator.arrival
import drt.shared.CrunchApi.{CrunchMinute, StaffMinute}
import drt.shared.FlightsApi.{Flights, FlightsWithSplits}
import drt.shared.PaxTypes.EeaMachineReadable
import drt.shared.Queues.EeaDesk
import drt.shared.SplitRatiosNs.SplitSources.TerminalAverage
import drt.shared.Terminals.T1
import drt.shared._
import drt.shared.api.Arrival
import passengersplits.core.PassengerTypeCalculatorValues.DocumentType
import passengersplits.parsing.VoyageManifestParser._
import server.feeds._
import services.SDate

import scala.collection.immutable.{List, SortedMap}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class ArrivalsGraphStageSpec extends CrunchTestLike {
  sequential
  isolated

  private val date = "2017-01-01"
  private val hour = "00:25"
  val scheduled = s"${date}T${hour}Z"

  val dateNow: SDateLike = SDate(date + "T00:00Z")
  val arrival_v1_with_no_chox_time: Arrival = arrival(iata = "BA0001", schDt = date + "T" + hour + "Z", actPax = Option(100), origin = PortCode("JFK"), feedSources = Set(LiveFeedSource))

  val arrival_v2_with_chox_time: Arrival = arrival_v1_with_no_chox_time.copy(Stand = Option("Stand1"), EstimatedChox = Option(SDate(date + "T" + hour + "Z").millisSinceEpoch))

  val terminalSplits: Splits = Splits(Set(ApiPaxTypeAndQueueCount(EeaMachineReadable, EeaDesk, 100.0, None, None)), TerminalAverage, None, Percentage)

  "Given and Arrivals Graph Stage" should {
    val airportConfig = defaultAirportConfig.copy(queuesByTerminal = defaultAirportConfig.queuesByTerminal.filterKeys(_ == T1))

    "a third arrival with an update to the chox time will change the arrival" >> {
      val crunch: CrunchGraphInputsAndProbes = runCrunchGraph(TestConfig(airportConfig = airportConfig, now = () => dateNow))
      val arrival_v3_with_an_update_to_chox_time: Arrival = arrival_v2_with_chox_time.copy(ActualChox = Option(SDate(scheduled).millisSinceEpoch), Stand = Option("I will update"))

      offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(Flights(Seq(arrival_v2_with_chox_time))))

      crunch.portStateTestProbe.fishForMessage(1 seconds) {
        case ps: PortState =>
          val arrivals = ps.flights.values.map(_.apiFlight)
          arrivals == Iterable(arrival_v2_with_chox_time)
      }

      offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(Flights(Seq(arrival_v3_with_an_update_to_chox_time))))

      val expectedArrivals: List[Arrival] = List(arrival_v3_with_an_update_to_chox_time)

      crunch.portStateTestProbe.fishForMessage(1 seconds) {
        case ps: PortState =>
          val arrivals = ps.flights.values.map(_.apiFlight)
          arrivals == expectedArrivals
      }

      success
    }

    "once an API (advanced passenger information) input arrives for the flight, it will update the arrivals FeedSource so that it has a LiveFeed and a ApiFeed" >> {
      val crunch: CrunchGraphInputsAndProbes = runCrunchGraph(TestConfig(airportConfig = airportConfig, now = () => dateNow))
      val voyageManifests: ManifestsFeedResponse = ManifestsFeedSuccess(DqManifests("", Set(
        VoyageManifest(EventTypes.DC, PortCode("STN"), PortCode("JFK"), VoyageNumber("0001"), CarrierCode("BA"), ManifestDateOfArrival(date), ManifestTimeOfArrival(hour), List(
          PassengerInfoJson(Option(DocumentType("P")), Nationality("GBR"), EeaFlag("EEA"), Option(PaxAge(22)), Option(PortCode("LHR")), InTransit("N"), Option(Nationality("GBR")), Option(Nationality("GBR")), None)
        ))
      )))

      offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(Flights(Seq(arrival_v2_with_chox_time))))

      crunch.portStateTestProbe.fishForMessage(1 second) {
        case PortState(flights, _, _) => flights.values.exists(_.apiFlight == arrival_v2_with_chox_time)
      }

      offerAndWait(crunch.manifestsLiveInput, voyageManifests)

      val expected = Set(LiveFeedSource, ApiFeedSource)

      crunch.portStateTestProbe.fishForMessage(1 seconds) {
        case ps: PortState =>
          val portStateSources = ps.flights.values.flatMap(_.apiFlight.FeedSources).toSet
          portStateSources == expected
      }

      success
    }

        "once an acl and a forecast input arrives for the flight, it will update the arrivals FeedSource so that it has ACLFeed and ForecastFeed" >> {
          val crunch: CrunchGraphInputsAndProbes = runCrunchGraph(TestConfig(airportConfig = airportConfig, now = () => dateNow))
          val forecastScheduled = "2017-01-01T10:25Z"

          val aclFlight: Flights = Flights(List(
            ArrivalGenerator.arrival(iata = "BA0002", schDt = forecastScheduled, actPax = Option(10), feedSources = Set(AclFeedSource))
          ))

          offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(Flights(Seq(arrival_v2_with_chox_time))))

          offerAndWait(crunch.aclArrivalsInput, ArrivalsFeedSuccess(aclFlight))

          val forecastArrival: Arrival = arrival(schDt = forecastScheduled, iata = "BA0002", terminal = T1, actPax = Option(21), feedSources = Set(ForecastFeedSource))
          val forecastArrivals: ArrivalsFeedResponse = ArrivalsFeedSuccess(Flights(List(forecastArrival)))

          offerAndWait(crunch.forecastArrivalsInput, forecastArrivals)

          val expected = Set(ForecastFeedSource, AclFeedSource)

          crunch.portStateTestProbe.fishForMessage(1 seconds) {
            case ps: PortState =>
              val portStateSources = ps.flights.get(forecastArrival.unique).map(_.apiFlight.FeedSources).getOrElse(Set())

              portStateSources == expected
          }

          success
        }

        "Given 2 arrivals, one international and the other domestic " +
          "I should only see the international arrival in the port state" >> {
          val crunch: CrunchGraphInputsAndProbes = runCrunchGraph(TestConfig(airportConfig = airportConfig, now = () => dateNow))
          val scheduled = "2017-01-01T10:25Z"

          val arrivalInt: Arrival = ArrivalGenerator.arrival(iata = "BA0002", origin = PortCode("JFK"), schDt = scheduled, actPax = Option(10), feedSources = Set(AclFeedSource))
          val arrivalDom: Arrival = ArrivalGenerator.arrival(iata = "BA0003", origin = PortCode("BHX"), schDt = scheduled, actPax = Option(10), feedSources = Set(AclFeedSource))

          val aclFlight: Flights = Flights(List(arrivalInt, arrivalDom))

          offerAndWait(crunch.aclArrivalsInput, ArrivalsFeedSuccess(aclFlight))

          crunch.portStateTestProbe.fishForMessage(1 seconds) {
            case ps: PortState => flightExists(arrivalInt, ps) && !flightExists(arrivalDom, ps)
          }

          success
        }
  }

    "Given an empty PortState I should only see arrivals without a suffix in the port state" >> {
      val withSuffixP: Arrival = ArrivalGenerator.arrival(iata = "BA0001P", origin = PortCode("JFK"), schDt = "2017-01-01T10:25Z", actPax = Option(10), feedSources = Set(AclFeedSource))
      val withSuffixF: Arrival = ArrivalGenerator.arrival(iata = "BA0002F", origin = PortCode("JFK"), schDt = "2017-01-01T11:25Z", actPax = Option(10), feedSources = Set(AclFeedSource))
      val withoutSuffix: Arrival = ArrivalGenerator.arrival(iata = "BA0003", origin = PortCode("JFK"), schDt = "2017-01-01T12:25Z", actPax = Option(10), feedSources = Set(AclFeedSource))

      "Given 3 international ACL arrivals, one with suffix F, another with P, and another with no suffix" >> {
        val crunch: CrunchGraphInputsAndProbes = runCrunchGraph(TestConfig(now = () => dateNow))
        val aclFlight: Flights = Flights(List(withSuffixP, withSuffixF, withoutSuffix))

        offerAndWait(crunch.aclArrivalsInput, ArrivalsFeedSuccess(aclFlight))

        crunch.portStateTestProbe.fishForMessage(1 seconds) {
          case ps: PortState =>
            val numberOfFlights = ps.flights.size
            numberOfFlights == 1 && flightExists(withoutSuffix, ps)
        }

        success
      }

      "Given 3 international live arrivals, one with suffix F, another with P, and another with no suffix" >> {
        val crunch: CrunchGraphInputsAndProbes = runCrunchGraph(TestConfig(now = () => dateNow))
        val aclFlight: Flights = Flights(List(withSuffixP, withSuffixF, withoutSuffix))

        offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(aclFlight))

        crunch.portStateTestProbe.fishForMessage(1 seconds) {
          case ps: PortState =>
            val numberOfFlights = ps.flights.size
            numberOfFlights == 1 && flightExists(withoutSuffix, ps)
        }

        success
      }
    }
//
    "Given a live arrival and a cirium arrival" >> {
      "When they have matching number, schedule, terminal and origin" >> {
        "I should see the live arrival with the cirium arrival's status merged" >> {
          val scheduled = "2021-06-01T12:00"
          val liveArrival = ArrivalGenerator.arrival("AA0001", schDt = scheduled, terminal = T1, origin = PortCode("AAA"))
          val ciriumArrival = ArrivalGenerator.arrival("AA0001", schDt = scheduled, terminal = T1, origin = PortCode("AAA"), estDt = scheduled)

          val crunch: CrunchGraphInputsAndProbes = runCrunchGraph(TestConfig(now = () => SDate(scheduled)))

          offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(Flights(List(liveArrival))))

          crunch.portStateTestProbe.fishForMessage(1 second) {
            case PortState(flights, _, _) => flights.nonEmpty
          }

          offerAndWait(crunch.ciriumArrivalsInput, ArrivalsFeedSuccess(Flights(List(ciriumArrival))))

          crunch.portStateTestProbe.fishForMessage(1 second) {
            case PortState(flights, _, _) => flights.values.exists(_.apiFlight.Estimated == Option(SDate(scheduled).millisSinceEpoch))
          }

          success
        }
      }

      "When they have matching number, schedule, terminal and origin" >> {
        "I should see the live arrival without the cirium arrival's status merged" >> {
          val scheduled = "2021-06-01T12:00"
          val liveArrival = ArrivalGenerator.arrival("AA0002", schDt = scheduled, terminal = T1, origin = PortCode("AAA"))
          val ciriumArrival = ArrivalGenerator.arrival("AA0002", schDt = scheduled, terminal = T1, origin = PortCode("BBB"), estDt = scheduled)

          val crunch: CrunchGraphInputsAndProbes = runCrunchGraph(TestConfig(now = () => SDate(scheduled)))

          offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(Flights(List(liveArrival))))

          crunch.portStateTestProbe.fishForMessage(1 second) {
            case PortState(flights, _, _) => flights.nonEmpty
          }

          offerAndWait(crunch.ciriumArrivalsInput, ArrivalsFeedSuccess(Flights(List(ciriumArrival))))

          Thread.sleep(500)

          val updatedChox = Option(SDate(scheduled).millisSinceEpoch)
          offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(Flights(List(liveArrival.copy(ActualChox = updatedChox)))))

          crunch.portStateTestProbe.fishForMessage(1 second) {
            case PortState(flights, _, _) => flights.values.exists { fws =>
              fws.apiFlight.ActualChox == updatedChox && fws.apiFlight.Estimated.isEmpty
            }
          }

          success
        }
      }
    }

  private def flightExists(withoutSuffix: Arrival, ps: PortState) = {
    ps.flights.contains(UniqueArrival(withoutSuffix))
  }
}
