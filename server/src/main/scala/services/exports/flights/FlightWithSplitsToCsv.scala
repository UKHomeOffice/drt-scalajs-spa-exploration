package services.exports.flights

import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.Queues.Queue
import drt.shared.{ApiFlightWithSplits, Queues}

object FlightWithSplitsToCsv {

  val arrivalHeadings = "IATA,ICAO,Origin,Gate/Stand,Status,Scheduled Date,Scheduled Time,Est Arrival,Act Arrival,Est Chox,Act Chox,Est PCP,Total Pax"
  val arrivalHeadingsWithTransfer: String = arrivalHeadings + ",Transfer Pax"

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

  private def headingsForSplitSource(queueNames: Seq[Queue], source: String): String = queueNames
    .map(q => s"$source ${Queues.queueDisplayNames(q)}")
    .mkString(",")

  def arrivalWithSplitsHeadings(queueNames: Seq[Queue]): String =
    arrivalHeadings + ",PCP Pax," +
      headingsForSplitSource(queueNames, "API") + "," +
      headingsForSplitSource(queueNames, "Historical") + "," +
      headingsForSplitSource(queueNames, "Terminal Average")

  def flightWithSplitsHeadingsPlusActualApi(queueNames: Seq[Queue]): String = arrivalWithSplitsHeadings(queueNames) + "," + actualApiHeadings.mkString(",")

  def flightWithSplitsToCsvFields(fws: ApiFlightWithSplits, millisToDateOnly: MillisSinceEpoch => String,
                                  millisToHoursAndMinutes: MillisSinceEpoch => String): List[String] =
    List(fws.apiFlight.flightCodeString,
      fws.apiFlight.flightCodeString,
      fws.apiFlight.Origin.toString,
      fws.apiFlight.Gate.getOrElse("") + "/" + fws.apiFlight.Stand.getOrElse(""),
      fws.apiFlight.displayStatus.description,
      millisToDateOnly(fws.apiFlight.Scheduled),
      millisToHoursAndMinutes(fws.apiFlight.Scheduled),
      fws.apiFlight.Estimated.map(millisToHoursAndMinutes(_)).getOrElse(""),
      fws.apiFlight.Actual.map(millisToHoursAndMinutes(_)).getOrElse(""),
      fws.apiFlight.EstimatedChox.map(millisToHoursAndMinutes(_)).getOrElse(""),
      fws.apiFlight.ActualChox.map(millisToHoursAndMinutes(_)).getOrElse(""),
      fws.apiFlight.PcpTime.map(millisToHoursAndMinutes(_)).getOrElse(""),
      fws.totalPax.getOrElse("").toString)
}
