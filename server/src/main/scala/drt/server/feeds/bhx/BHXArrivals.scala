package drt.server.feeds.bhx

import drt.shared.Arrival
import javax.xml.datatype.XMLGregorianCalendar
import org.joda.time.DateTime
import services.SDate
import uk.co.bhx.online.flightinformation.{FlightRecord, ScheduledFlightRecord}
import org.joda.time.DateTimeZone
import org.joda.time.format.ISODateTimeFormat

sealed trait BHXArrivals {

  def convertToUTC(feedDate: XMLGregorianCalendar): Option[String] = {
    val date = feedDate.toGregorianCalendar.getTime

    date.getTime match {
      case 0L => None
      case _ =>
        val datetime = new DateTime(date.getTime)
        Some(datetime.withZone(DateTimeZone.UTC).toString(ISODateTimeFormat.dateTime))
    }
  }

  def convertToUTCPlusOneHour(feedDate: XMLGregorianCalendar): String = {
    val utcDatePlusOneHour = new DateTime(feedDate.toGregorianCalendar.getTimeInMillis, DateTimeZone.UTC).plusHours(1)
    utcDatePlusOneHour.toString(ISODateTimeFormat.dateTime)
  }

}

trait BHXLiveArrivals extends BHXArrivals {

  def toLiveArrival(flightRecord: FlightRecord): Arrival =
    new Arrival(Operator = "",
      Status = flightRecord.getFlightStatus,
      EstDT = convertToUTC(flightRecord.getEstimatedTime).getOrElse(""),
      Estimated = convertToUTC(flightRecord.getEstimatedTime).map(SDate(_).millisSinceEpoch).getOrElse(0),
      ActDT = convertToUTC(flightRecord.getTouchdownTime).getOrElse(""),
      Actual = convertToUTC(flightRecord.getTouchdownTime).map(SDate(_).millisSinceEpoch).getOrElse(0),
      EstChoxDT = convertToUTC(flightRecord.getEstimatedChoxTime).getOrElse(""),
      EstimatedChox = convertToUTC(flightRecord.getEstimatedChoxTime).map(SDate(_).millisSinceEpoch).getOrElse(0),
      ActChoxDT = convertToUTC(flightRecord.getChoxTime).getOrElse(""),
      ActualChox = convertToUTC(flightRecord.getChoxTime).map(SDate(_).millisSinceEpoch).getOrElse(0),
      Gate = flightRecord.getGate,
      Stand = flightRecord.getStand,
      MaxPax = flightRecord.getCapacity,
      ActPax = flightRecord.getPassengers,
      TranPax = flightRecord.getTransits,
      RunwayID = flightRecord.getRunway,
      BaggageReclaimId = flightRecord.getBelt,
      FlightID = 0,
      AirportID = "BHX",
      Terminal = s"T${flightRecord.getTerminal}",
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
      Status = "Port Forecast",
      EstDT = "",
      Estimated = 0L,
      ActDT = "",
      Actual = 0L,
      EstChoxDT = "",
      EstimatedChox = 0L,
      ActChoxDT = "",
      ActualChox = 0L,
      Gate = "",
      Stand = "",
      MaxPax = flightRecord.getCapacity,
      ActPax = flightRecord.getPassengers,
      TranPax = flightRecord.getTransits,
      RunwayID = "",
      BaggageReclaimId = "",
      FlightID = 0,
      AirportID = "BHX",
      Terminal = s"T${flightRecord.getTerminal}",
      rawICAO = flightRecord.getFlightNumber,
      rawIATA = flightRecord.getFlightNumber,
      Origin = flightRecord.getOrigin,
      SchDT= convertToUTCPlusOneHour(flightRecord.getScheduledTime),
      Scheduled = SDate(convertToUTCPlusOneHour(flightRecord.getScheduledTime)).millisSinceEpoch,
      PcpTime = 0,
      None)
}