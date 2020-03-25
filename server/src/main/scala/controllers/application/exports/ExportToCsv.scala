package controllers.application.exports

import akka.NotUsed
import akka.actor.ActorRef
import akka.stream.scaladsl.Source
import akka.util.{ByteString, Timeout}
import controllers.Application
import drt.shared.Terminals.Terminal
import drt.shared.{PortCode, PortState, SDateLike}
import play.api.http.{HttpChunk, HttpEntity, Writeable}
import play.api.mvc.{ResponseHeader, Result}
import services.SDate
import services.exports.Exports
import services.exports.summaries.TerminalSummaryLike
import services.graphstages.Crunch.europeLondonTimeZone

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

trait ExportToCsv {
  self: Application =>

  implicit val timeout: Timeout = new Timeout(5 seconds)

  def exportToCsv(start: SDateLike,
                  end: SDateLike,
                  description: String,
                  terminal: Terminal,
                  maybeSummaryActorAndRequestProvider: Option[((SDateLike, Terminal) => ActorRef, Any)],
                  summaryFromPortState: (SDateLike, SDateLike, PortState) => Option[TerminalSummaryLike])
                 (implicit timeout: Timeout): Result = {
    if (airportConfig.terminals.toSet.contains(terminal)) {
      val startString = start.millisSinceEpoch.toString
      val endString = end.millisSinceEpoch.toString
      val exportSource = exportBetweenDates(startString, endString, terminal, description, maybeSummaryActorAndRequestProvider, summaryFromPortState)
      val fileName = makeFileName(description, terminal, start, end, airportConfig.portCode)

      Try(sourceToCsvResponse(exportSource, fileName)) match {
        case Success(value) => value
        case Failure(t) =>
          log.error("Failed to get CSV export", t)
          BadRequest("Failed to get CSV export")
      }
    } else {
      log.error(s"Bad terminal: $terminal")
      BadRequest(s"Invalid terminal $terminal")
    }
  }

  def exportBetweenDates(start: String,
                         end: String,
                         terminal: Terminal,
                         description: String,
                         maybeSummaryActorAndRequestProvider: Option[((SDateLike, Terminal) => ActorRef, Any)],
                         summaryFromPortStateProvider: (SDateLike, SDateLike, PortState) => Option[TerminalSummaryLike])
                        (implicit timeout: Timeout): Source[String, NotUsed] = {
    val startPit = SDate(start.toLong, europeLondonTimeZone).getLocalLastMidnight
    val endPit = SDate(end.toLong, europeLondonTimeZone).getLocalLastMidnight
    val numberOfDays = startPit.daysBetweenInclusive(endPit)

    log.info(s"Export $description for terminal $terminal between ${SDate(start.toLong).toISOString()} & ${SDate(end.toLong).toISOString()} ($numberOfDays days)")

    Exports.summaryForDaysCsvSource(startPit, numberOfDays, now, terminal, maybeSummaryActorAndRequestProvider, queryPortStateActor, summaryFromPortStateProvider)
  }

  def makeFileName(subject: String,
                   terminalName: Terminal,
                   startPit: SDateLike,
                   endPit: SDateLike,
                   portCode: PortCode): String = {
    val endDate = if (startPit != endPit)
      f"-to-${endPit.getFullYear()}-${endPit.getMonth()}%02d-${endPit.getDate()}%02d"
    else ""

    f"$portCode-$terminalName-$subject-" +
      f"${startPit.getFullYear()}-${startPit.getMonth()}%02d-${startPit.getDate()}%02d" + endDate
  }

  def sourceToCsvResponse(exportSource: Source[String, NotUsed], fileName: String): Result = {
    implicit val writeable: Writeable[String] = Writeable((str: String) => ByteString.fromString(str), Option("application/csv"))

    Result(
      header = ResponseHeader(200, Map("Content-Disposition" -> s"attachment; filename=$fileName.csv")),
      body = HttpEntity.Chunked(exportSource.map(c => HttpChunk.Chunk(writeable.transform(c))), writeable.contentType))
  }
}