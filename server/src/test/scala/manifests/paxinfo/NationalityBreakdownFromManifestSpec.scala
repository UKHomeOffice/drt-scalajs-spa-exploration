package manifests.paxinfo

import drt.shared._
import manifests.passengers.PassengerInfo
import manifests.paxinfo.ManifestBuilder.{manifestForPassengers, manifestWithPassengerNationalities, passengerBuilderWithOptions}
import org.specs2.mutable.Specification

import scala.collection.immutable.List

object NationalityBreakdownFromManifestSpec extends Specification {


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

  "When extracting nationality breakdown" >> {
    "Given a manifest with passengers of Unknown nationality" +
      "Then those passengers should be counted as Unknown rather than ignored" >> {

      val voyageManifest = manifestForPassengers(
        List(
          passengerBuilderWithOptions(nationality = None),
          passengerBuilderWithOptions(nationality = Option(Nationality("GBR"))),
        ))

      val result: Map[Nationality, Int] = PassengerInfo.manifestToNationalityCount(voyageManifest)

      val expected = Map(
        Nationality("Unknown") -> 1,
        Nationality("GBR") -> 1
      )

      result === expected
    }
  }

}
