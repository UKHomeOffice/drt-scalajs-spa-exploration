package services.exports.flights

import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.Queues
import drt.shared.Queues.Queue
import drt.shared.api.Arrival

object CedatArrivalToCsv {

  val arrivalHeadings = "IATA,ICAO,Origin,Gate/Stand,Status,Scheduled Date,Scheduled Time,Est Arrival,Act Arrival,Est Chox,Act Chox,Est PCP,Total Pax"

  val actualApiHeadings: Seq[String] = Seq(
    "API Actual - B5JSSK to Desk",
    "API Actual - B5JSSK to eGates",
    "API Actual - EEA (Machine Readable)",
    "API Actual - EEA (Non Machine Readable)",
    "API Actual - Fast Track (Non Visa)",
    "API Actual - Fast Track (Visa)",
    "API Actual - Non EEA (Non Visa)",
    "API Actual - Non EEA (Visa)",
    "API Actual - Transfer",
    "API Actual - eGates"
  )

  def headingsForSplitSource(queueNames: Seq[Queue], source: String): String = queueNames
    .map(q => s"$source ${Queues.queueDisplayNames(q)}")
    .mkString(",")

  def arrivalHeadings(queueNames: Seq[Queue]): String =
    arrivalHeadings + ",PCP Pax," +
      headingsForSplitSource(queueNames, "API") + "," +
      headingsForSplitSource(queueNames, "Historical") + "," +
      headingsForSplitSource(queueNames, "Terminal Average")

  def allHeadings(queueNames: Seq[Queue]): String = arrivalHeadings(queueNames) + "," + actualApiHeadings.mkString(",")

  def arrivalAsRawCsvValues(arrival: Arrival, millisToDateOnly: MillisSinceEpoch => String,
                            millisToHoursAndMinutes: MillisSinceEpoch => String): List[String] =
    List(arrival.flightCodeString,
      arrival.flightCodeString,
      arrival.Origin.toString,
      arrival.Gate.getOrElse("") + "/" + arrival.Stand.getOrElse(""),
      arrival.displayStatus.description,
      millisToDateOnly(arrival.Scheduled),
      millisToHoursAndMinutes(arrival.Scheduled),
      arrival.Estimated.map(millisToHoursAndMinutes(_)).getOrElse(""),
      arrival.Actual.map(millisToHoursAndMinutes(_)).getOrElse(""),
      arrival.EstimatedChox.map(millisToHoursAndMinutes(_)).getOrElse(""),
      arrival.ActualChox.map(millisToHoursAndMinutes(_)).getOrElse(""),
      arrival.PcpTime.map(millisToHoursAndMinutes(_)).getOrElse(""),
      arrival.ActPax.getOrElse("").toString)
}
