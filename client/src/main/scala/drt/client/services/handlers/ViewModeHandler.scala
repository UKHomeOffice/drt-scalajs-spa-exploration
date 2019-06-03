package drt.client.services.handlers

import diode._
import diode.data.Pot
import drt.client.actions.Actions._
import drt.client.services.JSDateConversions.SDate
import drt.client.services.{ViewDay, ViewLive, ViewMode}
import drt.shared.CrunchApi.{CrunchState, MillisSinceEpoch}
import drt.shared.SDateLike

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ViewModeHandler[M](viewModeCrunchStateMP: ModelRW[M, (ViewMode, Pot[CrunchState], MillisSinceEpoch)], crunchStateMP: ModelR[M, Pot[CrunchState]]) extends LoggingActionHandler(viewModeCrunchStateMP) {

  def midnightThisMorning: SDateLike = SDate.midnightOf(SDate.now())

  def isViewModeAbleToPoll(viewMode: ViewMode): Boolean =   viewMode match {
    case ViewLive() => true
    case ViewDay(time) if time.millisSinceEpoch >= midnightThisMorning.millisSinceEpoch => true
    case _ => false
  }

  protected def handle: PartialFunction[Any, ActionResult[M]] = {
    case SetViewMode(newViewMode) =>
      val (currentViewMode, _, _) = value

      (newViewMode, currentViewMode) match {
        case (newVm, oldVm) if newVm != oldVm =>
          updated((newViewMode, Pot.empty[CrunchState], 0L), initialRequests(newViewMode))
        case (ViewDay(newTime), ViewDay(oldTime)) if newTime != oldTime =>
          updated((newViewMode, Pot.empty[CrunchState], 0L), initialRequests(newViewMode))
        case _ =>
          noChange
      }
  }

  def initialRequests(newViewMode: ViewMode): EffectSet = {
    Effect(Future(GetInitialCrunchState(newViewMode))) +
      Effect(Future(GetStaffMovements(newViewMode))) +
      Effect(Future(GetShifts(newViewMode))) +
      Effect(Future(GetFixedPoints(newViewMode)))
  }
}
