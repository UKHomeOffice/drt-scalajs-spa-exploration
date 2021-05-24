package test.feeds.test

import drt.shared._
import drt.shared.api.Arrival
import passengersplits.core.PassengerTypeCalculatorValues.DocumentType
import passengersplits.parsing.VoyageManifestParser._
import services.SDate

object MockManifest {

  import VoyageManifestGenerator._

  def manifestForArrival(arrival: Arrival) = {
    val scheduled = SDate(arrival.Scheduled)
    val voyageManifest = VoyageManifestGenerator.voyageManifest(
      portCode = arrival.AirportID,
      scheduledDate = ManifestDateOfArrival(scheduled.toISODateOnly),
      scheduledTime = ManifestTimeOfArrival(scheduled.prettyTime()),
      paxInfos = List(
        List.fill(30)(euPassport),
        List.fill(10)(euChild),
        List.fill(20)(euIdCard),
        List.fill(10)(visa),
        List.fill(30)(nonVisa),
      ).flatten,
      departurePortCode = arrival.Origin,
      voyageNumber = arrival.VoyageNumber
    )

    voyageManifest
  }
}

object VoyageManifestGenerator {
  val euPassport = PassengerInfoJson(Option(DocumentType("P")), Nationality("GBR"), EeaFlag("EEA"), Option(PaxAge(22)), Option(PortCode("TEST")), InTransit("N"), Option(Nationality("GBR")), Option(Nationality("GBR")), None)
  val euChild = PassengerInfoJson(Option(DocumentType("P")), Nationality("GBR"), EeaFlag("EEA"), Option(PaxAge(11)), Option(PortCode("TEST")), InTransit("N"), Option(Nationality("GBR")), Option(Nationality("GBR")), None)

  def euPassportWithIdentifier(id: String) =
    PassengerInfoJson(Option(DocumentType("P")), Nationality("GBR"), EeaFlag("EEA"), Option(PaxAge(52)), Option(PortCode("TEST")), InTransit("N"), Option(Nationality("GBR")), Option(Nationality("GBR")), Option(id))

  val euIdCard = PassengerInfoJson(Option(DocumentType("I")), Nationality("ITA"), EeaFlag("EEA"), Option(PaxAge(22)), Option(PortCode("TEST")), InTransit("N"), Option(Nationality("GBR")), Option(Nationality("ITA")), None)
  val visa = PassengerInfoJson(Option(DocumentType("P")), Nationality("EGY"), EeaFlag("EEA"), Option(PaxAge(22)), Option(PortCode("TEST")), InTransit("N"), Option(Nationality("GBR")), Option(Nationality("AFG")), None)
  val nonVisa = PassengerInfoJson(Option(DocumentType("P")), Nationality("SLV"), EeaFlag("EEA"), Option(PaxAge(22)), Option(PortCode("TEST")), InTransit("N"), Option(Nationality("GBR")), Option(Nationality("ALA")), None)
  val inTransitFlag = PassengerInfoJson(Option(DocumentType("P")), Nationality("GBR"), EeaFlag("EEA"), Option(PaxAge(22)), Option(PortCode("TEST")), InTransit("Y"), Option(Nationality("GBR")), Option(Nationality("GBR")), None)
  val inTransitCountry = PassengerInfoJson(Option(DocumentType("P")), Nationality("GBR"), EeaFlag("EEA"), Option(PaxAge(22)), Option(PortCode("JFK")), InTransit("N"), Option(Nationality("GBR")), Option(Nationality("GBR")), None)

  def manifestPax(qty: Int, passport: PassengerInfoJson): List[PassengerInfoJson] = {
    List.fill(qty)(passport)
  }

  def voyageManifest(dqEventCode: EventType = EventTypes.DC,
                     portCode: PortCode = PortCode("TEST"),
                     departurePortCode: PortCode = PortCode("JFK"),
                     voyageNumber: VoyageNumber = VoyageNumber(0),
                     carrierCode: CarrierCode = CarrierCode("BA"),
                     scheduledDate: ManifestDateOfArrival = ManifestDateOfArrival("2017-01-01"),
                     scheduledTime: ManifestTimeOfArrival = ManifestTimeOfArrival("00:00"),
                     paxInfos: List[PassengerInfoJson] = List()): VoyageManifest = {
    VoyageManifest(dqEventCode, portCode, departurePortCode, voyageNumber, carrierCode, scheduledDate, scheduledTime, paxInfos)
  }
}
