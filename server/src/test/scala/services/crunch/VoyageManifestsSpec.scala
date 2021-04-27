package services.crunch

import controllers.ArrivalGenerator
import drt.shared.FlightsApi.Flights
import drt.shared.PaxTypes.EeaMachineReadable
import drt.shared.PaxTypesAndQueues._
import drt.shared.Queues._
import drt.shared.SplitRatiosNs.SplitSources.ApiSplitsWithHistoricalEGateAndFTPercentages
import drt.shared.Terminals.T1
import drt.shared._
import manifests.passengers.{BestAvailableManifest, ManifestPassengerProfile}
import passengersplits.core.PassengerTypeCalculatorValues.DocumentType
import passengersplits.parsing.VoyageManifestParser
import passengersplits.parsing.VoyageManifestParser._
import server.feeds.{ArrivalsFeedSuccess, DqManifests, ManifestsFeedSuccess}
import services.SDate
import services.crunch.VoyageManifestGenerator._

import scala.collection.immutable.{List, Map, Seq, SortedMap}
import scala.concurrent.duration._


class VoyageManifestsSpec extends CrunchTestLike {
  sequential
  isolated

  "Given 2 DQ messages for a flight, where the DC message arrives after the CI message " +
    "When I crunch the flight " +
    "Then I should see the DQ manifest was used" >> {

    val scheduled = "2017-01-01T00:00Z"

    val flight = ArrivalGenerator.arrival(origin = PortCode("JFK"), schDt = scheduled, iata = "TST001", terminal = T1, actPax = Option(1))
    val inputManifestsCi = ManifestsFeedSuccess(DqManifests("", Set(
      VoyageManifest(EventTypes.CI, PortCode("STN"), PortCode("JFK"), VoyageNumber("0001"), CarrierCode("TS"), ManifestDateOfArrival("2017-01-01"), ManifestTimeOfArrival("00:00"),
        List(
          PassengerInfoGenerator.passengerInfoJson(Nationality("GBR"), DocumentType("P"), Nationality("GBR"))
        ))
    )))
    val inputManifestsDc = ManifestsFeedSuccess(DqManifests("", Set(
      VoyageManifest(EventTypes.DC, PortCode("STN"), PortCode("JFK"), VoyageNumber("0001"), CarrierCode("TS"), ManifestDateOfArrival("2017-01-01"), ManifestTimeOfArrival("00:00"),
        List(
          PassengerInfoGenerator.passengerInfoJson(Nationality("ZAF"), DocumentType("P"), Nationality("ZAF"))
        ))
    )))
    val crunch = runCrunchGraph(TestConfig(
      now = () => SDate(scheduled),
      airportConfig = defaultAirportConfig.copy(
        slaByQueue = Map(Queues.EGate -> 15, Queues.EeaDesk -> 25, Queues.NonEeaDesk -> 45),
        terminalProcessingTimes = Map(T1 -> Map(
          eeaMachineReadableToDesk -> 25d / 60,
          eeaMachineReadableToEGate -> 25d / 60,
          nonVisaNationalToDesk -> 25d / 60
        )),
        queuesByTerminal = SortedMap(T1 -> Seq(EeaDesk, EGate, NonEeaDesk))
      )
    ))

    offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(Flights(Seq(flight))))
    offerAndWait(crunch.manifestsLiveInput, inputManifestsCi)
    Thread.sleep(1000)
    offerAndWait(crunch.manifestsLiveInput, inputManifestsDc)

    val expectedNonZeroQueues = Set(NonEeaDesk)

    crunch.portStateTestProbe.fishForMessage(3 seconds) {
      case ps: PortState =>
        val nonZeroQueues = ps.crunchMinutes.values.filter(_.paxLoad > 0).groupBy(_.queue).keys.toSet
        nonZeroQueues == expectedNonZeroQueues
    }

    success
  }

  "Given a VoyageManifest and its arrival where the arrival has a different number of passengers to the manifest but is within 5%" >> {
    "When I crunch the flight " >> {
      "Then I should see the passenger loads corresponding to the manifest splits applied to the arrival's passengers" >> {

        val scheduled = "2017-01-01T00:00Z"
        val portCode = PortCode("LHR")

        val flight = ArrivalGenerator.arrival(origin = PortCode("JFK"), schDt = scheduled, iata = "TST001", terminal = T1, actPax = Option(100))
        val inputManifests = ManifestsFeedSuccess(DqManifests("", Set(
          VoyageManifest(EventTypes.CI, portCode, PortCode("JFK"), VoyageNumber("0001"), CarrierCode("BA"), ManifestDateOfArrival("2017-01-01"), ManifestTimeOfArrival("00:00"),
            manifestPax(101, euPassport))
        )))
        val crunch = runCrunchGraph(TestConfig(
          now = () => SDate(scheduled),
          airportConfig = defaultAirportConfig.copy(
            slaByQueue = Map(Queues.EGate -> 15, Queues.EeaDesk -> 25, Queues.NonEeaDesk -> 45),
            portCode = portCode,
            terminalProcessingTimes = Map(T1 -> Map(
              eeaMachineReadableToDesk -> 25d / 60,
              eeaMachineReadableToEGate -> 25d / 60
            )),
            terminalPaxTypeQueueAllocation = defaultAirportConfig.terminalPaxTypeQueueAllocation.updated(
              T1, defaultAirportConfig.terminalPaxTypeQueueAllocation(T1).updated(EeaMachineReadable, List(Queues.EGate -> 0.8, Queues.EeaDesk -> 0.2),
              )
            ),
            queuesByTerminal = SortedMap(T1 -> Seq(EeaDesk, EGate))
          )
        ))

        offerAndWait(crunch.aclArrivalsInput, ArrivalsFeedSuccess(Flights(Seq(flight))))
        offerAndWait(crunch.manifestsLiveInput, inputManifests)

        val expected = Map(Queues.EeaDesk -> 4.0, Queues.EGate -> 16.0)

        crunch.portStateTestProbe.fishForMessage(3 seconds) {
          case ps: PortState =>
            val queuePax = ps.crunchMinutes
              .values
              .filter(cm => cm.minute == SDate(scheduled).millisSinceEpoch)
              .map(cm => (cm.queue, cm.paxLoad))
              .toMap

            queuePax == expected
        }

        success
      }
    }
  }

  "Given a VoyageManifest with 2 transfers and one Eea Passport " >> {
    "When I crunch the flight with 10 pax minus 5 transit " >> {
      "Then I should see the 5 non-transit pax go to the egates" >> {

        val scheduled = "2017-01-01T00:00Z"
        val portCode = PortCode("LHR")
        val flight = ArrivalGenerator.arrival(origin = PortCode("JFK"), schDt = scheduled, iata = "TST001", terminal = T1, actPax = Option(10), tranPax = Option(5))
        val inputManifests = ManifestsFeedSuccess(DqManifests("", Set(
          VoyageManifest(EventTypes.CI, portCode, PortCode("JFK"), VoyageNumber("0001"), CarrierCode("BA"), ManifestDateOfArrival("2017-01-01"), ManifestTimeOfArrival("00:00"),
            manifestPax(5, euPassport) ++ manifestPax(2, inTransitFlag) ++ manifestPax(3, inTransitCountry)
          )
        )))
        val crunch = runCrunchGraph(TestConfig(
          now = () => SDate(scheduled),
          airportConfig = defaultAirportConfig.copy(
            slaByQueue = Map(Queues.EGate -> 15, Queues.EeaDesk -> 25, Queues.NonEeaDesk -> 45),
            portCode = portCode,
            terminalProcessingTimes = Map(T1 -> Map(
              eeaMachineReadableToDesk -> 25d / 60,
              eeaMachineReadableToEGate -> 25d / 60
            )),
            terminalPaxTypeQueueAllocation = defaultAirportConfig.terminalPaxTypeQueueAllocation.updated(
              T1, defaultAirportConfig.terminalPaxTypeQueueAllocation(T1).updated(EeaMachineReadable, List(Queues.EGate -> 0.8, Queues.EeaDesk -> 0.2),
              )
            ),
            queuesByTerminal = SortedMap(T1 -> Seq(EeaDesk, EGate, NonEeaDesk)),
            hasTransfer = true
          )
        ))

        offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(Flights(Seq(flight))))
        offerAndWait(crunch.manifestsLiveInput, inputManifests)

        val expected = Map(Queues.EeaDesk -> 1.0, Queues.EGate -> 4.0, Queues.NonEeaDesk -> 0.0)

        crunch.portStateTestProbe.fishForMessage(3 seconds) {
          case ps: PortState =>
            val queuePax = ps.crunchMinutes
              .values
              .filter(cm => cm.minute == SDate(scheduled).millisSinceEpoch)
              .map(cm => (cm.queue, cm.paxLoad))
              .toMap

            queuePax == expected
        }

        success
      }
    }
  }

  "Given a voyage manifest then I should get a BestAvailableManifest that matches it" >> {
    val vm = VoyageManifest(EventTypes.CI, PortCode("LHR"), PortCode("JFK"), VoyageNumber("0001"), CarrierCode("BA"), ManifestDateOfArrival("2017-01-01"), ManifestTimeOfArrival("00:00"), List(
      inTransitFlag,
      inTransitCountry,
      euPassport,
      euIdCard,
      visa,
      visa
    ))

    val result = BestAvailableManifest(vm)

    val expected = BestAvailableManifest(
      ApiSplitsWithHistoricalEGateAndFTPercentages, PortCode("LHR"), PortCode("JFK"), VoyageNumber("0001"), CarrierCode("BA"), SDate("2017-01-01"),
      List(
        ManifestPassengerProfile(Nationality("GBR"), Option(DocumentType("P")), Option(PaxAge(22)), Option(true), None),
        ManifestPassengerProfile(Nationality("GBR"), Option(DocumentType("P")), Option(PaxAge(22)), Option(true), None),
        ManifestPassengerProfile(Nationality("GBR"), Option(DocumentType("P")), Option(PaxAge(22)), Option(false), None),
        ManifestPassengerProfile(Nationality("ITA"), Option(DocumentType("I")), Option(PaxAge(22)), Option(false), None),
        ManifestPassengerProfile(Nationality("AFG"), Option(DocumentType("P")), Option(PaxAge(22)), Option(false), None),
        ManifestPassengerProfile(Nationality("AFG"), Option(DocumentType("P")), Option(PaxAge(22)), Option(false), None)
      )
    )

    result === expected
  }

  "Given a voyage manifest `Passport` instead of `P` for doctype it should still be accepted as a passport doctype" >> {
    val vm = VoyageManifest(EventTypes.CI, PortCode("LHR"), PortCode("JFK"), VoyageNumber("0001"), CarrierCode("BA"), ManifestDateOfArrival("2017-01-01"), ManifestTimeOfArrival("00:00"), List(
      PassengerInfoJson(Option(DocumentType("Passport")), Nationality("GBR"), EeaFlag("EEA"), Option(PaxAge(22)), Option(PortCode("LHR")), InTransit("N"), Option(Nationality("GBR")), Option(Nationality("GBR")), None)
    ))

    val result = BestAvailableManifest(vm)

    val expected = BestAvailableManifest(
      ApiSplitsWithHistoricalEGateAndFTPercentages, PortCode("LHR"), PortCode("JFK"), VoyageNumber("0001"), CarrierCode("BA"), SDate("2017-01-01"),
      List(
        ManifestPassengerProfile(Nationality("GBR"), Option(DocumentType.Passport), Option(PaxAge(22)), Option(false), None)
      )
    )

    result === expected
  }

  "Given a voyage manifest with a UK National and no doctype, Passport should be assumed" >> {
    val vm = VoyageManifest(EventTypes.CI, PortCode("LHR"), PortCode("JFK"), VoyageNumber("0001"), CarrierCode("BA"), ManifestDateOfArrival("2017-01-01"), ManifestTimeOfArrival("00:00"), List(
      PassengerInfoJson(None, Nationality("GBR"), EeaFlag("EEA"), Option(PaxAge(22)), Option(PortCode("LHR")), InTransit("N"), Option(Nationality("GBR")), Option(Nationality("GBR")), None)
    ))

    val result = BestAvailableManifest(vm)

    val expected = BestAvailableManifest(
      ApiSplitsWithHistoricalEGateAndFTPercentages, PortCode("LHR"), PortCode("JFK"), VoyageNumber("0001"), CarrierCode("BA"), SDate("2017-01-01"),
      List(
        ManifestPassengerProfile(Nationality("GBR"), Option(DocumentType.Passport), Option(PaxAge(22)), Option(false), None)
      )
    )

    result === expected
  }

  "Given a VoyageManifest with 2 transfers, 1 Eea Passport, 1 Eea Id card, and 2 visa nationals " >> {
    "When I crunch the flight with 4 non-transit pax (10 pax minus 6 transit) " >> {
      "Then I should see the 4 non-transit pax go to egates (1), eea desk (1), and non-eea (2)" >> {

        val scheduled = "2017-01-01T00:00Z"
        val portCode = PortCode("LHR")

        val flight = ArrivalGenerator.arrival(origin = PortCode("JFK"), schDt = scheduled, iata = "TST001", terminal = T1, actPax = Option(10), tranPax = Option(6))
        val inputManifests = ManifestsFeedSuccess(DqManifests("", Set(
          VoyageManifest(EventTypes.CI, portCode, PortCode("JFK"), VoyageNumber("0001"), CarrierCode("TS"), ManifestDateOfArrival("2017-01-01"), ManifestTimeOfArrival("00:00"), List(
            inTransitFlag,
            inTransitCountry,
            euPassport,
            euIdCard,
            visa,
            visa
          ))
        )))
        val crunch = runCrunchGraph(TestConfig(
          now = () => SDate(scheduled),
          airportConfig = defaultAirportConfig.copy(
            slaByQueue = Map(Queues.EGate -> 15, Queues.EeaDesk -> 25, Queues.NonEeaDesk -> 45),
            portCode = portCode,
            terminalProcessingTimes = Map(T1 -> Map(
              eeaMachineReadableToDesk -> 25d / 60,
              eeaNonMachineReadableToDesk -> 25d / 60,
              eeaMachineReadableToEGate -> 25d / 60,
              visaNationalToDesk -> 25d / 60
            )),
            terminalPaxTypeQueueAllocation = defaultAirportConfig.terminalPaxTypeQueueAllocation.updated(
              T1, defaultAirportConfig.terminalPaxTypeQueueAllocation(T1).updated(EeaMachineReadable, List(Queues.EGate -> 0.8, Queues.EeaDesk -> 0.2),
              )
            ),
            queuesByTerminal = SortedMap(T1 -> Seq(EeaDesk, EGate, NonEeaDesk)),
            hasTransfer = true
          )
        ))

        offerAndWait(crunch.liveArrivalsInput, ArrivalsFeedSuccess(Flights(Seq(flight))))
        offerAndWait(crunch.manifestsLiveInput, inputManifests)

        val expected = Map(Queues.EeaDesk -> 1.2, Queues.EGate -> 0.8, Queues.NonEeaDesk -> 2.0)

        crunch.portStateTestProbe.fishForMessage(10 seconds) {
          case ps: PortState =>
            val queuePax = ps.crunchMinutes
              .values
              .filter(cm => cm.minute == SDate(scheduled).millisSinceEpoch)
              .map(cm => (cm.queue, cm.paxLoad))
              .toMap

            queuePax == expected
        }

        success
      }
    }
  }

  "Given a VoyageManifest with multiple records for each passenger " +
    "I should get a BestAvailableManifests with records for each unique passenger identifier only" >> {

    val scheduled = "2017-01-01T00:00Z"
    val portCode = PortCode("LHR")

    val flight = ArrivalGenerator.arrival(origin = PortCode("JFK"), schDt = scheduled, iata = "TST001", terminal = T1, actPax = None, tranPax = None)
    val inputManifests = ManifestsFeedSuccess(DqManifests("", Set(
      VoyageManifest(EventTypes.CI, portCode, PortCode("JFK"), VoyageNumber(1), CarrierCode("TS"), ManifestDateOfArrival("2017-01-01"), ManifestTimeOfArrival("00:00"), List(
        euPassportWithIdentifier("ID1"),
        euPassportWithIdentifier("ID1"),
        euPassportWithIdentifier("ID2")
      ))
    )))
    val crunch = runCrunchGraph(TestConfig(
      now = () => SDate(scheduled),
      airportConfig = defaultAirportConfig.copy(
        slaByQueue = Map(Queues.EGate -> 15, Queues.EeaDesk -> 25, Queues.NonEeaDesk -> 45),
        portCode = portCode,
        terminalProcessingTimes = Map(T1 -> Map(
          eeaMachineReadableToDesk -> 25d / 60,
          eeaNonMachineReadableToDesk -> 25d / 60,
          eeaMachineReadableToEGate -> 25d / 60,
          visaNationalToDesk -> 25d / 60
        )),
        queuesByTerminal = SortedMap(T1 -> Seq(EeaDesk, EGate, NonEeaDesk))
      )
    ))

    offerAndWait(crunch.aclArrivalsInput, ArrivalsFeedSuccess(Flights(Seq(flight))))
    offerAndWait(crunch.manifestsLiveInput, inputManifests)

    val expected = 2

    crunch.portStateTestProbe.fishForMessage(1 seconds) {
      case ps: PortState =>
        val queuePax = paxLoadsFromPortState(ps, 60, 0)
          .values
          .flatMap(_.values)
          .flatten
          .sum

        Math.round(queuePax) == expected
    }

    success
  }

  "Given a VoyageManifest with multiple records for each passenger and no Passenger Identifier" +
    "I should get a BestAvailableManifests with records for each entry in the passenger list" >> {

    val scheduled = "2017-01-01T00:00Z"
    val portCode = PortCode("LHR")

    val flight = ArrivalGenerator.arrival(origin = PortCode("JFK"), schDt = scheduled, iata = "TST001", terminal = T1, actPax = None, tranPax = Option(6))
    val inputManifests = ManifestsFeedSuccess(DqManifests("", Set(
      VoyageManifest(EventTypes.CI, portCode, PortCode("JFK"), VoyageNumber(1), CarrierCode("TS"), ManifestDateOfArrival("2017-01-01"), ManifestTimeOfArrival("00:00"), List(
        euPassport,
        euPassport,
        euPassport,
      ))
    )))
    val crunch = runCrunchGraph(TestConfig(
      now = () => SDate(scheduled),
      airportConfig = defaultAirportConfig.copy(
        slaByQueue = Map(Queues.EGate -> 15, Queues.EeaDesk -> 25, Queues.NonEeaDesk -> 45),
        portCode = portCode,
        terminalProcessingTimes = Map(T1 -> Map(
          eeaMachineReadableToDesk -> 25d / 60,
          eeaNonMachineReadableToDesk -> 25d / 60,
          eeaMachineReadableToEGate -> 25d / 60,
          visaNationalToDesk -> 25d / 60
        )),
        queuesByTerminal = SortedMap(T1 -> Seq(EeaDesk, EGate, NonEeaDesk))
      )
    ))

    offerAndWait(crunch.aclArrivalsInput, ArrivalsFeedSuccess(Flights(Seq(flight))))
    offerAndWait(crunch.manifestsLiveInput, inputManifests)

    val expected = 3

    crunch.portStateTestProbe.fishForMessage(1 seconds) {
      case ps: PortState =>
        val queuePax = paxLoadsFromPortState(ps, 60, 0)
          .values
          .flatMap((_.values))
          .flatten
          .sum

        Math.round(queuePax) == expected
    }

    success
  }

  "Given a VoyageManifest containing some records with a passenger identifier, we should ignore all records without one." >> {

    val scheduled = "2017-01-01T00:00Z"
    val portCode = PortCode("LHR")

    val flight = ArrivalGenerator.arrival(origin = PortCode("JFK"), schDt = scheduled, iata = "TST001", terminal = T1, actPax = None, tranPax = Option(6))
    val inputManifests = ManifestsFeedSuccess(DqManifests("", Set(
      VoyageManifest(EventTypes.CI, portCode, PortCode("JFK"), VoyageNumber(1), CarrierCode("TS"), ManifestDateOfArrival("2017-01-01"), ManifestTimeOfArrival("00:00"), List(
        euPassportWithIdentifier("Id1"),
        euPassportWithIdentifier("Id2"),
        euPassport,
        euPassport,
      ))
    )))
    val crunch = runCrunchGraph(TestConfig(
      now = () => SDate(scheduled),
      airportConfig = defaultAirportConfig.copy(
        slaByQueue = Map(Queues.EGate -> 15, Queues.EeaDesk -> 25, Queues.NonEeaDesk -> 45),
        portCode = portCode,
        terminalProcessingTimes = Map(T1 -> Map(
          eeaMachineReadableToDesk -> 25d / 60,
          eeaNonMachineReadableToDesk -> 25d / 60,
          eeaMachineReadableToEGate -> 25d / 60,
          visaNationalToDesk -> 25d / 60
        )),
        queuesByTerminal = SortedMap(T1 -> Seq(EeaDesk, EGate, NonEeaDesk))
      )
    ))

    offerAndWait(crunch.aclArrivalsInput, ArrivalsFeedSuccess(Flights(Seq(flight))))
    offerAndWait(crunch.manifestsLiveInput, inputManifests)

    val expected = 2

    crunch.portStateTestProbe.fishForMessage(1 seconds) {
      case ps: PortState =>
        val queuePax = paxLoadsFromPortState(ps, 60, 0)
          .values
          .flatMap((_.values))
          .flatten
          .sum

        Math.round(queuePax) == expected
    }

    success
  }

  "Given a voyage manifest json string I should get back a correctly parsed VoyageManifest" >> {
    val manifestString =
      """|{
         |    "EventCode": "DC",
         |    "DeparturePortCode": "AMS",
         |    "VoyageNumberTrailingLetter": "",
         |    "ArrivalPortCode": "TST",
         |    "DeparturePortCountryCode": "MAR",
         |    "VoyageNumber": "0123",
         |    "VoyageKey": "key",
         |    "ScheduledDateOfDeparture": "2020-09-07",
         |    "ScheduledDateOfArrival": "2020-09-07",
         |    "CarrierType": "AIR",
         |    "CarrierCode": "TS",
         |    "ScheduledTimeOfDeparture": "06:30:00",
         |    "ScheduledTimeOfArrival": "09:30:00",
         |    "FileId": "fileID",
         |    "PassengerList": [
         |        {
         |            "DocumentIssuingCountryCode": "GBR",
         |            "PersonType": "P",
         |            "DocumentLevel": "Primary",
         |            "Age": "30",
         |            "DisembarkationPortCode": "TST",
         |            "InTransitFlag": "N",
         |            "DisembarkationPortCountryCode": "TST",
         |            "NationalityCountryEEAFlag": "EEA",
         |            "PassengerIdentifier": "id",
         |            "DocumentType": "P",
         |            "PoavKey": "1",
         |            "NationalityCountryCode": "GBR"
         |         }
         |    ]
         |}""".stripMargin

    val result = VoyageManifestParser.parseVoyagePassengerInfo(manifestString).get

    val expected = VoyageManifest(
      EventCode = EventType("DC"),
      ArrivalPortCode = PortCode("TST"),
      DeparturePortCode = PortCode("AMS"),
      VoyageNumber = VoyageNumber(123),
      CarrierCode = CarrierCode("TS"),
      ScheduledDateOfArrival = ManifestDateOfArrival("2020-09-07"),
      ScheduledTimeOfArrival = ManifestTimeOfArrival("09:30:00"),
      PassengerList = List(PassengerInfoJson(
        DocumentType = Option(DocumentType("P")),
        DocumentIssuingCountryCode = Nationality("GBR"),
        EEAFlag = EeaFlag("EEA"),
        Age = Option(PaxAge(30)),
        DisembarkationPortCode = Option(PortCode("TST")),
        InTransitFlag = InTransit("N"),
        DisembarkationPortCountryCode = Some(Nationality("TST")),
        NationalityCountryCode = Some(Nationality("GBR")),
        PassengerIdentifier = Option("id")
      ))
    )

    result === expected
  }
}

object PassengerInfoGenerator {
  def passengerInfoJson(nationality: Nationality, documentType: DocumentType, issuingCountry: Nationality): PassengerInfoJson = {
    PassengerInfoJson(Option(documentType), issuingCountry, EeaFlag(""), Option(PaxAge(22)), Option(PortCode("LHR")), InTransit("N"), Option(Nationality("GBR")), Option(nationality), None)
  }
}
