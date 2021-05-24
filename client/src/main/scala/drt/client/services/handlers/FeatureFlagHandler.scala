package drt.client.services.handlers

import diode.data.{Pending, Pot, Ready}
import diode.{ActionResult, Effect, ModelRW}
import drt.client.actions.Actions._
import drt.client.services.{DrtApi, PollDelay}
import drt.shared.FeatureFlags
import upickle.default.read

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

class FeatureFlagHandler[M](modelRW: ModelRW[M, Pot[FeatureFlags]]) extends LoggingActionHandler(modelRW) {
  protected def handle: PartialFunction[Any, ActionResult[M]] = {
    case GetFeatureFlags =>
      updated(Pending(), Effect(DrtApi.get("feature-flags")
        .map(r => UpdateFeatureFlags(read[FeatureFlags](r.responseText))).recover {
        case _ =>
          RetryActionAfter(GetFeatureFlags, PollDelay.recoveryDelay)
      }))
    case UpdateFeatureFlags(featureFlags) =>
      updated(Ready(featureFlags), Effect(Future(RetryActionAfter(GetFeatureFlags, PollDelay.checkFeatureFlagsDelay))))
  }
}
