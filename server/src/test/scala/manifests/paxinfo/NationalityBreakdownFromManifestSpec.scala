package manifests.paxinfo

import drt.shared._
import manifests.passengers.PassengerInfo
import org.specs2.mutable.Specification
import passengersplits.core.PassengerTypeCalculatorValues.DocumentType
import passengersplits.parsing.VoyageManifestParser._

import scala.collection.immutable.List

object NationalityBreakdownFromManifestSpec extends Specification {

  def manifestWithPassengerNationalities(nats: List[String]): VoyageManifest = {
    VoyageManifest(EventTypes.DC,
      PortCode("TST"),
      PortCode("JFK"),
      VoyageNumber("0001"),
      CarrierCode("BA"),
      ManifestDateOfArrival("2020-11-09"),
      ManifestTimeOfArrival("00:00"),
      nats.map(nat =>
        PassengerInfoJson(
          DocumentType = Option(DocumentType("P")),
          DocumentIssuingCountryCode = Nationality(nat),
          EEAFlag = EeaFlag("EEA"),
          Age = Option(PaxAge(22)),
          DisembarkationPortCode = Option(PortCode("LHR")),
          InTransitFlag = InTransit("N"),
          DisembarkationPortCountryCode = Option(Nationality(nat)),
          NationalityCountryCode = Option(Nationality(nat)),
          PassengerIdentifier = None
        )
      ))
  }

  "When extracting nationality breakdown" >> {
    "Given a manifest with 1 passenger with a nationality of GB " +
      "Then I should get a Map of GBR to 1" >> {

      val voyageManifest = manifestWithPassengerNationalities(List("GBR"))
      val result = PassengerInfo.manifestToNationalityCount(voyageManifest)

      val expected = Map(Nationality("GBR") -> 1)

      result === expected
    }
  }

  "When extracting nationality breakdown" >> {
    "Given a manifest with multiple GB passengers " +
      "Then I should see the total of all GB Pax for that nationality" >> {

      val voyageManifest = manifestWithPassengerNationalities(List("GBR", "GBR", "GBR"))

      val result = PassengerInfo.manifestToNationalityCount(voyageManifest)

      val expected = Map(Nationality("GBR") -> 3)

      result === expected
    }
  }

  "When extracting nationality breakdown" >> {
    "Given a manifest with multiple nationalities" +
      "Then I should see the total of each nationality across all queues" >> {

      val voyageManifest = manifestWithPassengerNationalities(
        List(
          "MRU",
          "AUS",
          "GBR",
          "GBR",
          "ZWE",
          "GBR",
          "AUS",
        ))

      val result = PassengerInfo.manifestToNationalityCount(voyageManifest)

      val expected = Map(
        Nationality("AUS") -> 2,
        Nationality("GBR") -> 3,
        Nationality("MRU") -> 1,
        Nationality("ZWE") -> 1
      )

      result === expected
    }
  }

}