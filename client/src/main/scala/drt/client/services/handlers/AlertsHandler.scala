package drt.client.services.handlers

import autowire._
import diode.Implicits.runAfterImpl
import diode.data._
import diode.{ActionResult, Effect, ModelRW}
import drt.client.actions.Actions._
import drt.client.logger.log
import drt.client.services.JSDateConversions.SDate
import drt.client.services.{AjaxClient, DrtApi, PollDelay}
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared.{Alert, Api}
import upickle.default.read
import upickle.default.write

import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import boopickle.Default._

class AlertsHandler[M](modelRW: ModelRW[M, Pot[Seq[Alert]]]) extends LoggingActionHandler(modelRW) {

  val alertsRequestFrequency: FiniteDuration = 10 seconds
  protected def handle: PartialFunction[Any, ActionResult[M]] = {

    case GetAlerts(since: MillisSinceEpoch) =>
      effectOnly(Effect(DrtApi.get(s"alerts/$since").map(r => {
        val alerts = read[Seq[Alert]](r.responseText)
        SetAlerts(alerts, since)
      }).recoverWith {
        case _ =>
          log.info(s"Alerts request failed. Re-requesting after ${PollDelay.recoveryDelay}")
          Future(RetryActionAfter(GetLoggedInUser, PollDelay.recoveryDelay))
      }))

    case SetAlerts(alerts, since) =>
      val effect = Effect(Future(GetAlerts(since))).after(alertsRequestFrequency)
      val pot = if (alerts.isEmpty) Empty else Ready(alerts)
      if (modelRW.value.isPending && since <= SDate.now().addMinutes(-1).millisSinceEpoch) {
        noChange
      } else {
        updated(pot, effect)
      }

    case DeleteAllAlerts =>
      val responseFuture = DrtApi.delete("alerts")
        .map(_ => DoNothing())
        .recoverWith {
          case _ =>
            log.error(s"Failed to delete all alerts. Re-requesting after ${PollDelay.recoveryDelay}")
            Future(RetryActionAfter(DeleteAllAlerts, PollDelay.recoveryDelay))
        }
      effectOnly(Effect(responseFuture))

    case SaveAlert(alert) =>
      val responseFuture = DrtApi.post("alerts", write(alert))
        .map(_ => DoNothing())
        .recoverWith {
          case _ =>
            log.error(s"Failed to save Alert. Re-requesting after ${PollDelay.recoveryDelay}")
            Future(RetryActionAfter(SaveAlert(alert), PollDelay.recoveryDelay))
        }
      effectOnly(Effect(responseFuture))
  }
}
