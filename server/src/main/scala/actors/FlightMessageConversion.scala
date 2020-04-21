package actors

import actors.PortStateMessageConversion.splitMessageToApiSplits
import actors.restore.RestorerWithLegacy
import drt.shared.Terminals.Terminal
import drt.shared._
import drt.shared.api.{Arrival, FlightCodeSuffix}
import org.apache.commons.lang3.StringUtils
import org.slf4j.{Logger, LoggerFactory}
import server.protobuf.messages.CrunchState.{FlightWithSplitsMessage, PaxTypeAndQueueCountMessage, SplitMessage}
import server.protobuf.messages.FlightsMessage.{FeedStatusMessage, FeedStatusesMessage, FlightMessage, FlightStateSnapshotMessage}
import services.SDate

import scala.util.{Success, Try}

object FlightMessageConversion {
  val log: Logger = LoggerFactory.getLogger(getClass.toString)

  def arrivalsStateToSnapshotMessage(state: ArrivalsState): FlightStateSnapshotMessage = {
    val maybeStatusMessages = state.maybeSourceStatuses.flatMap(feedStatuses => feedStatusesToMessage(feedStatuses.feedStatuses))

    FlightStateSnapshotMessage(
      state.arrivals.values.map(apiFlightToFlightMessage).toSeq,
      maybeStatusMessages
    )
  }

  def feedStatusesToMessage(statuses: FeedStatuses): Option[FeedStatusesMessage] = {
    val statusMessages = statuses.statuses.map(feedStatusToMessage)

    Option(FeedStatusesMessage(statusMessages, statuses.lastSuccessAt, statuses.lastFailureAt, statuses.lastUpdatesAt))
  }

  def feedStatusToMessage(feedStatus: FeedStatus): FeedStatusMessage = feedStatus match {
    case s: FeedStatusSuccess => FeedStatusMessage(Option(s.date), Option(s.updateCount), None)
    case s: FeedStatusFailure => FeedStatusMessage(Option(s.date), None, Option(s.message))
  }

  def restoreArrivalsFromSnapshot(restorer: RestorerWithLegacy[Int, UniqueArrival, Arrival],
                                  snMessage: FlightStateSnapshotMessage): Unit = {
    restorer.update(snMessage.flightMessages.map(flightMessageToApiFlight))
  }

  def feedStatusesFromSnapshotMessage(snMessage: FlightStateSnapshotMessage): Option[FeedStatuses] = {
    snMessage.statuses.map(feedStatusesFromFeedStatusesMessage)
  }

  def feedStatusesFromFeedStatusesMessage(message: FeedStatusesMessage): FeedStatuses = FeedStatuses(
    statuses = message.statuses.map(feedStatusFromFeedStatusMessage).toList,
    lastSuccessAt = message.lastSuccessAt,
    lastFailureAt = message.lastFailureAt,
    lastUpdatesAt = message.lastUpdatesAt
  )

  def feedStatusFromFeedStatusMessage(message: FeedStatusMessage): FeedStatus = {
    if (message.updates.isDefined)
      FeedStatusSuccess(message.date.getOrElse(0L), message.updates.getOrElse(0))
    else
      FeedStatusFailure(message.date.getOrElse(0L), message.message.getOrElse("n/a"))
  }

  def flightWithSplitsToMessage(f: ApiFlightWithSplits): FlightWithSplitsMessage = {
    FlightWithSplitsMessage(
      Option(FlightMessageConversion.apiFlightToFlightMessage(f.apiFlight)),
      f.splits.map(apiSplitsToMessage).toList,
      lastUpdated = f.lastUpdated)
  }

  def flightWithSplitsFromMessage(fm: FlightWithSplitsMessage): ApiFlightWithSplits = ApiFlightWithSplits(
    FlightMessageConversion.flightMessageToApiFlight(fm.flight.get),
    fm.splits.map(sm => splitMessageToApiSplits(sm)).toSet,
    lastUpdated = fm.lastUpdated
    )

  def apiSplitsToMessage(s: Splits): SplitMessage = {
    SplitMessage(
      paxTypeAndQueueCount = s.splits.map(paxTypeAndQueueCountToMessage).toList,
      source = Option(s.source.toString),
      eventType = s.maybeEventType.map(_.toString),
      style = Option(s.splitStyle.name)
    )
  }

  def paxTypeAndQueueCountToMessage(ptqc: ApiPaxTypeAndQueueCount): PaxTypeAndQueueCountMessage = {
    PaxTypeAndQueueCountMessage(
      Option(ptqc.passengerType.name),
      Option(ptqc.queueType.toString),
      Option(ptqc.paxCount)
    )
  }

  def apiFlightToFlightMessage(apiFlight: Arrival): FlightMessage = {
    FlightMessage(
      operator = apiFlight.Operator.map(_.code),
      gate = apiFlight.Gate,
      stand = apiFlight.Stand,
      status = Option(apiFlight.Status.description),
      maxPax = apiFlight.MaxPax.filter(_ != 0),
      actPax = apiFlight.ActPax.filter(_ != 0),
      tranPax = apiFlight.TranPax,
      runwayID = apiFlight.RunwayID,
      baggageReclaimId = apiFlight.BaggageReclaimId,
      airportID = Option(apiFlight.AirportID.iata),
      terminal = Option(apiFlight.Terminal.toString),
      iCAO = Option(apiFlight.flightCode),
      iATA = Option(apiFlight.flightCode),
      origin = Option(apiFlight.Origin.toString),
      pcpTime = apiFlight.PcpTime.filter(_ != 0),
      feedSources = apiFlight.FeedSources.map(_.toString).toSeq,
      scheduled = Option(apiFlight.Scheduled).filter(_ != 0),
      estimated = apiFlight.Estimated.filter(_ != 0),
      touchdown = apiFlight.Actual.filter(_ != 0),
      estimatedChox = apiFlight.EstimatedChox.filter(_ != 0),
      actualChox = apiFlight.ActualChox.filter(_ != 0),
      carrierScheduled = apiFlight.CarrierScheduled,
      apiPax = apiFlight.ApiPax
    )
  }

  def millisOptionFromArrivalDateString(datetime: String): Option[Long] = datetime match {
    case "" => None
    case _ =>
      Try {
        SDate.parseString(datetime)
      } match {
        case Success(MilliDate(millis)) => Some(millis)
        case _ => None
      }
  }

  def flightMessageToApiFlight(flightMessage: FlightMessage): Arrival = {
    Arrival(
      Operator = flightMessage.operator.map(Operator),
      Status = ArrivalStatus(flightMessage.status.getOrElse("")),
      Estimated = flightMessage.estimated,
      Actual = flightMessage.touchdown,
      EstimatedChox = flightMessage.estimatedChox,
      ActualChox = flightMessage.actualChox,
      Gate = flightMessage.gate,
      Stand = flightMessage.stand,
      MaxPax = flightMessage.maxPax,
      ActPax = flightMessage.actPax,
      TranPax = flightMessage.tranPax,
      RunwayID = flightMessage.runwayID,
      BaggageReclaimId = flightMessage.baggageReclaimId,
      AirportID = PortCode(flightMessage.airportID.getOrElse("")),
      Terminal = Terminal(flightMessage.terminal.getOrElse("")),
      rawICAO = flightMessage.iCAO.getOrElse(""),
      rawIATA = flightMessage.iATA.getOrElse(""),
      Origin = PortCode(flightMessage.origin.getOrElse("")),
      PcpTime = flightMessage.pcpTime,
      Scheduled = flightMessage.scheduled.getOrElse(0L),
      FeedSources = flightMessage.feedSources.flatMap(FeedSource(_)).toSet,
      CarrierScheduled = flightMessage.carrierScheduled,
      ApiPax = flightMessage.apiPax
      )
  }

  def apiFlightDateTime(millisOption: Option[Long]): String = millisOption match {
    case Some(millis: Long) => SDate.jodaSDateToIsoString(SDate(millis))
    case _ => ""
  }
}
