package actors

import drt.shared.Terminals.T1
import drt.shared.api.Arrival
import drt.shared.{FeedSource, Operator, PortCode}
import org.specs2.mutable.Specification
import server.protobuf.messages.FlightsMessage.FlightMessage

class FlightMessageConversionSpec extends Specification {
  "Given a protobuf FlightMessage " +
    "When converting it to an Arrival " +
    "Then I should see all the relevant fields populated" >> {

    val operator = "BA"
    val gate = "G1"
    val stand = "S1"
    val status = "landed"
    val maxPax = 350
    val actPax = 122
    val runwayId = "R1"
    val baggageReclaimId = "B1"
    val airportId = PortCode("LHR")
    val terminal = T1
    val icao = "BAA1111"
    val iata = "BA1111"
    val origin = PortCode("JFK")
    val pcpTime = 10L
    val scheduledTime = 1L
    val estimatedTime = 2L
    val touchdownTime = 3L
    val estimatedChoxTime = 4L
    val actualChoxTime = 5L
    val feedSources = Set("ACL", "Live")
    val transPax = 10

    val carrierScheduled = 4L

    val flightMessage = new FlightMessage(
      operator = Option(operator),
      gate = Option(gate),
      stand = Option(stand),
      status = Option(status),
      maxPax = Option(maxPax),
      actPax = Option(actPax),
      tranPax = Option(transPax),
      runwayID = Option(runwayId),
      baggageReclaimId = Option(baggageReclaimId),
      airportID = Option(airportId.iata),
      terminal = Option(terminal.toString),
      iCAO = Option(icao),
      iATA = Option(iata),
      origin = Option(origin.iata),
      pcpTime = Option(pcpTime),
      scheduled = Option(scheduledTime),
      estimated = Option(estimatedTime),
      touchdown = Option(touchdownTime),
      estimatedChox = Option(estimatedChoxTime),
      actualChox = Option(actualChoxTime),
      feedSources = feedSources.toSeq,
      carrierScheduled = Option(carrierScheduled)
    )

    val arrival = FlightMessageConversion.flightMessageToApiFlight(flightMessage)

    import drt.server.feeds.Implicits._

    val expected = Arrival(
      Operator = Option(Operator(operator)),
      Status = status,
      Estimated = Option(estimatedTime),
      Actual = Option(touchdownTime),
      EstimatedChox = Option(estimatedChoxTime),
      ActualChox = Option(actualChoxTime),
      Gate = Option(gate),
      Stand = Option(stand),
      MaxPax = Option(maxPax),
      ActPax = Option(actPax),
      TranPax = Option(transPax),
      RunwayID = Option(runwayId),
      BaggageReclaimId = Option(baggageReclaimId),
      AirportID = airportId,
      Terminal = terminal,
      rawICAO = icao,
      rawIATA = iata,
      Origin = origin,
      Scheduled = scheduledTime,
      PcpTime = Option(pcpTime),
      FeedSources = feedSources.map(fs => FeedSource(fs)).collect { case Some(fs) => fs },
      CarrierScheduled = Option(carrierScheduled)
    )

    arrival === expected
  }
}
