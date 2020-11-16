package manifests.paxinfo

import drt.shared._
import drt.shared.api.AgeRange
import manifests.passengers.PassengerInfo
import org.specs2.mutable.Specification
import passengersplits.core.PassengerTypeCalculatorValues.DocumentType
import passengersplits.parsing.VoyageManifestParser._

import scala.collection.immutable.List

object PaxSplitsDataFromManifestsSpec extends Specification {

  def manifestWithPassengerAges(ages: List[Int]): VoyageManifest = {
    VoyageManifest(EventTypes.DC,
      PortCode("TST"),
      PortCode("JFK"),
      VoyageNumber("0001"),
      CarrierCode("BA"),
      ManifestDateOfArrival("2020-11-09"),
      ManifestTimeOfArrival("00:00"),
      ages.map(age =>
        PassengerInfoJson(Option(DocumentType("P")),
          Nationality("GBR"),
          EeaFlag("EEA"),
          Option(PaxAge(age)),
          Option(PortCode("LHR")),
          InTransit("N"),
          Option(Nationality("GBR")),
          Option(Nationality("GBR")),
          None
        )
      ))
  }

  "When extracting Age data from a manifest" >> {
    "Given a voyage manifest with 1 passenger aged 11" +
      "Then I should get back age data with 1 passenger in the 0-11 age range" >> {

      val manifest = manifestWithPassengerAges(List(11))

      val result = PassengerInfo.manifestToAgeRangeCount(manifest)

      val expected = Map(AgeRange(0, Option(11)) -> 1)


      result === expected
    }

    "Given a voyage manifest with 3 passengers aged 11, 12 and 30" +
      "Then I should get back age data with 1 passenger each in the 0-11, 11-24 and 25-40 ranges" >> {

      val apiSplit = manifestWithPassengerAges(List(11, 12, 30))

      val result = PassengerInfo.manifestToAgeRangeCount(apiSplit)

      val expected = Map(
          AgeRange(0, Option(11)) -> 1,
          AgeRange(12, Option(24)) -> 1,
          AgeRange(25, Option(49)) -> 1
        )


      result === expected
    }
  }

  "Given a manifest containing multiple passengers in each range" +
    "Then passengers should be summed across age ranges for each split" >> {

    val manifest = manifestWithPassengerAges(List(
      11, 11, 11,
      12, 24,
      30,26,45,
      55,
      100, 99
    ))


    val result = PassengerInfo.manifestToAgeRangeCount(manifest)

    val expected = Map(
      AgeRange(0, Option(11)) -> 3,
      AgeRange(12, Option(24)) -> 2,
      AgeRange(25, Option(49)) -> 3,
      AgeRange(50, Option(65)) -> 1,
      AgeRange(65, None) -> 2
    )

    println(result)
    println(expected)

    result === expected
  }

}
