package actors

import drt.shared._
import org.apache.commons.lang3.StringUtils
import org.slf4j.{Logger, LoggerFactory}
import server.protobuf.messages.CrunchState.{FlightWithSplitsMessage, PaxTypeAndQueueCountMessage, SplitMessage}
import server.protobuf.messages.FlightsMessage.{FeedStatusMessage, FeedStatusesMessage, FlightMessage, FlightStateSnapshotMessage}
import services.SDate

import scala.util.{Success, Try}

object FlightMessageConversion {
  val log: Logger = LoggerFactory.getLogger(getClass.toString)

  def arrivalsStateToSnapshotMessage(state: ArrivalsState): FlightStateSnapshotMessage = {
    val maybeStatusMessages = state.maybeFeedStatuses.flatMap(feedStatuses => feedStatusesToMessage(feedStatuses))

    FlightStateSnapshotMessage(
      state.arrivals.values.map(apiFlightToFlightMessage).toSeq,
      maybeStatusMessages
    )
  }

  def feedStatusesToMessage(statuses: FeedStatuses): Option[FeedStatusesMessage] = {
    val statusMessages = statuses.statuses.map(feedStatusToMessage)

    Option(FeedStatusesMessage(Option(statuses.name), statusMessages, statuses.lastSuccessAt, statuses.lastFailureAt, statuses.lastUpdatesAt))
  }

  def feedStatusToMessage(feedStatus: FeedStatus): FeedStatusMessage = feedStatus match {
    case s: FeedStatusSuccess => FeedStatusMessage(Option(s.date), Option(s.updateCount), None)
    case s: FeedStatusFailure => FeedStatusMessage(Option(s.date), None, Option(s.message))
  }

  def arrivalsStateFromSnapshotMessage(snMessage: FlightStateSnapshotMessage, feedName: String): ArrivalsState = {
    val arrivalsMap = snMessage.flightMessages.map(fm => {
      val arrival = FlightMessageConversion.flightMessageToApiFlight(fm)
      (arrival.uniqueId, arrival)
    }).toMap

    val maybeStatuses = snMessage.statuses.map(feedStatusesFromFeedStatusesMessage)

    ArrivalsState(arrivalsMap, feedName, maybeStatuses)
  }

  def feedStatusesFromFeedStatusesMessage(message: FeedStatusesMessage): FeedStatuses = {
    FeedStatuses(
      name = message.name.getOrElse("n/a"),
      statuses = message.statuses.map(feedStatusFromFeedStatusMessage).toList,
      lastSuccessAt = message.lastSuccessAt,
      lastFailureAt = message.lastFailureAt,
      lastUpdatesAt = message.lastUpdatesAt
    )
  }

  def feedStatusFromFeedStatusMessage(message: FeedStatusMessage): FeedStatus = {
    if (message.updates.isDefined)
      FeedStatusSuccess(message.date.getOrElse(0L), message.updates.getOrElse(0))
    else
      FeedStatusFailure(message.date.getOrElse(0L), message.message.getOrElse("n/a"))
  }

  def flightWithSplitsToMessage(f: ApiFlightWithSplits): FlightWithSplitsMessage = {
    FlightWithSplitsMessage(
      Option(FlightMessageConversion.apiFlightToFlightMessage(f.apiFlight)),
      f.splits.map(apiSplitsToMessage).toList)
  }

  def apiSplitsToMessage(s: ApiSplits): SplitMessage = {
    SplitMessage(
      paxTypeAndQueueCount = s.splits.map(paxTypeAndQueueCountToMessage).toList,
      source = Option(s.source),
      eventType = s.eventType,
      style = Option(s.splitStyle.name)
    )
  }

  def paxTypeAndQueueCountToMessage(ptqc: ApiPaxTypeAndQueueCount): PaxTypeAndQueueCountMessage = {
    PaxTypeAndQueueCountMessage(
      Option(ptqc.passengerType.name),
      Option(ptqc.queueType),
      Option(ptqc.paxCount)
    )
  }

  def apiFlightToFlightMessage(apiFlight: Arrival): FlightMessage = {
    FlightMessage(
      operator = apiFlight.Operator,
      gate = apiFlight.Gate,
      stand = apiFlight.Stand,
      status = Option(StringUtils.trimToNull(apiFlight.Status)),
      maxPax = apiFlight.MaxPax.filter(_ != 0),
      actPax = apiFlight.ActPax.filter(_ != 0),
      tranPax = apiFlight.TranPax,
      runwayID = apiFlight.RunwayID,
      baggageReclaimId = apiFlight.BaggageReclaimId,
      flightID = apiFlight.FlightID.filter(_ != 0),
      airportID = Option(StringUtils.trimToNull(apiFlight.AirportID)),
      terminal = Option(StringUtils.trimToNull(apiFlight.Terminal)),
      iCAO = Option(StringUtils.trimToNull(apiFlight.rawICAO)),
      iATA = Option(StringUtils.trimToNull(apiFlight.rawIATA)),
      origin = Option(StringUtils.trimToNull(apiFlight.Origin)),
      pcpTime = apiFlight.PcpTime.filter(_ != 0),
      feedSources = apiFlight.FeedSources.map(_.toString).toSeq,
      scheduled = Option(apiFlight.Scheduled).filter(_ != 0),
      estimated = apiFlight.Estimated.filter(_ != 0),
      touchdown = apiFlight.Actual.filter(_ != 0),
      estimatedChox = apiFlight.EstimatedChox.filter(_ != 0),
      actualChox = apiFlight.ActualChox.filter(_ != 0)
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
      Operator = flightMessage.operator,
      Status = flightMessage.status.getOrElse(""),
      Estimated = flightMessage.estimated.filter(_ != 0),
      Actual = flightMessage.touchdown.filter(_ != 0),
      EstimatedChox = flightMessage.estimatedChox.filterNot(_ != 0),
      ActualChox = flightMessage.actualChox.filter(_ != 0),
      Gate = flightMessage.gate,
      Stand = flightMessage.stand,
      MaxPax = flightMessage.maxPax.filter(_ != 0),
      ActPax = flightMessage.actPax.filter(_ != 0),
      TranPax = flightMessage.tranPax,
      RunwayID = flightMessage.runwayID,
      BaggageReclaimId = flightMessage.baggageReclaimId,
      FlightID = flightMessage.flightID.filter(_ != 0),
      AirportID = flightMessage.airportID.getOrElse(""),
      Terminal = flightMessage.terminal.getOrElse(""),
      rawICAO = flightMessage.iCAO.getOrElse(""),
      rawIATA = flightMessage.iATA.getOrElse(""),
      Origin = flightMessage.origin.getOrElse(""),
      PcpTime = flightMessage.pcpTime.filter(_ != 0),
      LastKnownPax = flightMessage.lastKnownPax,
      Scheduled = flightMessage.scheduled.getOrElse(0L),
      FeedSources = flightMessage.feedSources.flatMap(FeedSource(_)).toSet
    )
  }

  def apiFlightDateTime(millisOption: Option[Long]): String = millisOption match {
    case Some(millis: Long) => SDate.jodaSDateToIsoString(SDate(millis))
    case _ => ""
  }
}
