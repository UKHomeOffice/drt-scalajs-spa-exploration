package drt.client.services.handlers

import diode.Implicits.runAfterImpl
import diode.data.{Pot, Ready}
import diode.{ActionResult, Effect, ModelRW, NoAction}
import drt.client.actions.Actions._
import drt.client.logger.log
import drt.client.services.JSDateConversions.SDate
import drt.client.services._
import drt.shared._
import upickle.default.{read, write}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

class FixedPointsHandler[M](getCurrentViewMode: () => ViewMode, modelRW: ModelRW[M, Pot[FixedPointAssignments]]) extends LoggingActionHandler(modelRW) {
  def scheduledRequest(viewMode: ViewMode): Effect = Effect(Future(GetFixedPoints(viewMode))).after(2 seconds)

  protected def handle: PartialFunction[Any, ActionResult[M]] = {
    case SetFixedPoints(viewMode, fixedPoints, _) =>
      if (viewMode.isHistoric(SDate.now()))
        updated(Ready(fixedPoints))
      else
        updated(Ready(fixedPoints), scheduledRequest(viewMode))

    case SaveFixedPoints(assignments, terminalName) =>
      log.info(s"Calling saveFixedPoints")

      val otherTerminalFixedPoints = value.getOrElse(FixedPointAssignments.empty).notForTerminal(terminalName)
      val newFixedPoints: FixedPointAssignments = assignments + otherTerminalFixedPoints
      val futureResponse = DrtApi.post("fixed-points", write(newFixedPoints))
        .map(_ => NoAction)
        .recoverWith {
          case _ =>
            log.error(s"Failed to save FixedPoints. Re-requesting after ${PollDelay.recoveryDelay}")
            Future(RetryActionAfter(SaveFixedPoints(assignments, terminalName), PollDelay.recoveryDelay))
        }
      effectOnly(Effect(futureResponse))

    case GetFixedPoints(viewMode) if viewMode.isDifferentTo(getCurrentViewMode()) =>
      log.info(s"Ignoring old view response")
      noChange

    case GetFixedPoints(viewMode) =>
      val url = if (viewMode.isHistoric(SDate.now())) {
        s"fixed-points?pointInTime=${viewMode.millis}"
      } else {
        "fixed-points"
      }

      val apiCallEffect = Effect(
        DrtApi.get(url)
          .map(r => SetFixedPoints(viewMode, read[FixedPointAssignments](r.responseText), None))
          .recoverWith {
            case _ =>
              log.error(s"Failed to get fixed points. Polling will continue")
              Future(NoAction)
          }
      )
      effectOnly(apiCallEffect)
  }
}
