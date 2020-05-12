package drt.client.services.handlers

import diode.Implicits.runAfterImpl
import diode._
import diode.data._
import drt.client.actions.Actions._
import drt.client.logger._
import drt.client.services.JSDateConversions.SDate
import drt.client.services._
import drt.shared.CrunchApi._
import drt.shared.PortState
import org.scalajs.dom
import upickle.default.read

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

class InitialPortStateHandler[M](getCurrentViewMode: () => ViewMode,
                                 modelRW: ModelRW[M, (Pot[PortState], MillisSinceEpoch)]) extends LoggingActionHandler(modelRW) {
  val crunchUpdatesRequestFrequency: FiniteDuration = 2 seconds

  val thirtySixHoursInMillis: Long = 1000L * 60 * 60 * 36

  def handle: PartialFunction[Any, ActionResult[M]] = {
    case GetInitialPortState(viewMode) =>
      val startMillis = viewMode.dayStart.millisSinceEpoch
      val endMillis = startMillis + thirtySixHoursInMillis
      val updateRequestFuture = viewMode match {
        case ViewPointInTime(time) => DrtApi.get(s"crunch-snapshot/${time.millisSinceEpoch}?start=$startMillis&end=$endMillis")
        case _ => DrtApi.get(s"crunch?start=$startMillis&end=$endMillis")
      }

      val eventualAction = processRequest(viewMode, updateRequestFuture)

      updated((Pending(), 0L), Effect(Future(ShowLoader())) + Effect(eventualAction))

    case SetPortState(viewMode, _) if viewMode.isDifferentTo(getCurrentViewMode()) =>
      log.info(s"Ignoring out of date view response")
      noChange

    case SetPortState(viewMode, portState) =>
      log.info(s"Got a crunch state! ${portState.flights.size} flights, ${portState.crunchMinutes.size} crunch minutes, ${portState.staffMinutes.size} staff minutes")
      val originCodes = portState.flights
        .map { case (_, fws) => fws.apiFlight.Origin }
        .toSet

      val hideLoader = Effect(Future(HideLoader()))
      val fetchOrigins = Effect(Future(GetAirportInfos(originCodes)))

      val effects = if (getCurrentViewMode().isHistoric) {
        hideLoader + fetchOrigins
      } else {
        log.info(s"Starting to poll for crunch updates")
        hideLoader + fetchOrigins + getCrunchUpdatesAfterDelay(viewMode)
      }

      updated((Ready(portState), portState.latestUpdate), effects)
  }

  def processRequest(viewMode: ViewMode, call: Future[dom.XMLHttpRequest]): Future[Action] = {
    call
      .map { r =>
        read[PortState](r.responseText)
      }
      .map { portState =>
          log.info(s"Got and set an initial PortState")
          SetPortState(viewMode, portState)
//        case Left(error) =>
//          log.error(s"Failed to GetInitialPortState ${error.message}")
//          if (viewMode.isDifferentTo(getCurrentViewMode())) {
//            log.info(s"No need to request as view has changed")
//            NoAction
//          } else {
//            log.info(s"Re-requesting initial PortState after ${PollDelay.recoveryDelay}")
//            RetryActionAfter(GetInitialPortState(viewMode), PollDelay.recoveryDelay)
//          }
      }
      .recoverWith {
        case throwable =>
          log.error(s"Call to crunch-state failed (${throwable.getMessage}. Re-requesting after ${PollDelay.recoveryDelay}")
          Future(RetryActionAfter(GetInitialPortState(viewMode), PollDelay.recoveryDelay))
      }
  }

  def getCrunchUpdatesAfterDelay(viewMode: ViewMode): Effect = Effect(Future(GetPortStateUpdates(viewMode))).after(crunchUpdatesRequestFrequency)
}
