package drt.server.feeds.bhx

import drt.shared.Arrival
import javax.xml.datatype.XMLGregorianCalendar
import org.joda.time.DateTime
import services.SDate
import uk.co.bhx.online.flightinformation.{FlightRecord, ScheduledFlightRecord}

sealed trait BHXArrivals {

  def convertToUTC(feedDate: XMLGregorianCalendar): Option[String] = {
    val date = feedDate.toGregorianCalendar.getTime

    date.getTime match {
      case 0L => None
      case _ =>
        val datetime = new DateTime(date.getTime)

        import org.joda.time.DateTimeZone
        import org.joda.time.format.ISODateTimeFormat

        Some(datetime.withZone(DateTimeZone.UTC).toString(ISODateTimeFormat.dateTime))
    }
  }

}

trait BHXLiveArrivals extends BHXArrivals {

  def toLiveArrival(flightRecord: FlightRecord): Arrival =
    new Arrival(Operator = "",
      Status = flightRecord.getFlightStatus match {
        case "Arrived" => "A"
        case "Expected" => "E"
        case "Cancelled" => "C"
        case _ => "U"
      },
      EstDT = convertToUTC(flightRecord.getEstimatedTime).getOrElse(""),
      ActDT = convertToUTC(flightRecord.getTouchdownTime).getOrElse(""),
      EstChoxDT = convertToUTC(flightRecord.getEstimatedChoxTime).getOrElse(""),
      ActChoxDT = convertToUTC(flightRecord.getChoxTime).getOrElse(""),
      Gate = flightRecord.getGate,
      Stand = flightRecord.getStand,
      MaxPax = flightRecord.getCapacity,
      ActPax = flightRecord.getPassengers,
      TranPax = flightRecord.getTransits,
      RunwayID = flightRecord.getRunway,
      BaggageReclaimId = flightRecord.getBelt,
      FlightID = 0,
      AirportID = "BHX",
      Terminal = flightRecord.getTerminal,
      rawICAO = flightRecord.getFlightNumber,
      rawIATA = flightRecord.getFlightNumber,
      Origin = flightRecord.getOrigin,
      SchDT = convertToUTC(flightRecord.getScheduledTime).getOrElse(""),
      Scheduled = convertToUTC(flightRecord.getScheduledTime).map(SDate(_).millisSinceEpoch).getOrElse(0),
      PcpTime = 0,
      None)
}

trait BHXForecastArrivals extends BHXArrivals {

  def toForecastArrival(flightRecord: ScheduledFlightRecord) : Arrival =
    new Arrival(Operator = "",
      Status = "S",
      EstDT = "",
      ActDT = "",
      EstChoxDT = "",
      ActChoxDT = "",
      Gate = "",
      Stand = "",
      MaxPax = flightRecord.getCapacity,
      ActPax = flightRecord.getPassengers,
      TranPax = flightRecord.getTransits,
      RunwayID = "",
      BaggageReclaimId = "",
      FlightID = 0,
      AirportID = "BHX",
      Terminal = flightRecord.getTerminal,
      rawICAO = flightRecord.getFlightNumber,
      rawIATA = flightRecord.getFlightNumber,
      Origin = flightRecord.getOrigin,
      SchDT= convertToUTC(flightRecord.getScheduledTime).getOrElse(""),
      Scheduled = convertToUTC(flightRecord.getScheduledTime).map(SDate(_).millisSinceEpoch).getOrElse(0),
      PcpTime = 0,
      None)
}