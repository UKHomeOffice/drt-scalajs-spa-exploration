package controllers.application

import actors.{GetPortState, GetUpdatesSince, PointInTimeQuery}
import akka.pattern.ask
import controllers.Application
import drt.auth.DesksAndQueuesView
import drt.shared.CrunchApi.{MillisSinceEpoch, PortStateUpdates}
import drt.shared.PortState
import play.api.mvc.{Action, AnyContent, Request}
import services.SDate
import upickle.default.write

import scala.concurrent.Future


trait WithPortState {
  self: Application =>

  def getCrunch: Action[AnyContent] = authByRole(DesksAndQueuesView) {
    Action.async { request: Request[AnyContent] =>
      val startMillis = request.queryString.get("start").flatMap(_.headOption.map(_.toLong)).getOrElse(0L)
      val endMillis = request.queryString.get("end").flatMap(_.headOption.map(_.toLong)).getOrElse(0L)

      val maybeSinceMillis = request.queryString.get("since").flatMap(_.headOption.map(_.toLong))

      val maybePointInTime = if (endMillis < SDate.now().millisSinceEpoch) {
        val oneHourMillis = 1000 * 60 * 60
        Option(endMillis + oneHourMillis * 2)
      } else None

      val eventualUpdates = maybeSinceMillis match {
        case None =>
          futurePortState(maybePointInTime, GetPortState(startMillis, endMillis)).map(r => Ok(write(r)))
        case Some(sinceMillis) =>
          futureUpdates(GetUpdatesSince(sinceMillis, startMillis, endMillis)).map(r => Ok(write(r)))
      }

      eventualUpdates
        .recoverWith {
          case t =>
            log.error(t, "Error processing request for port state or port state updates")
            Future(InternalServerError)
        }
    }
  }

  def getCrunchSnapshot(pointInTime: MillisSinceEpoch): Action[AnyContent] = authByRole(DesksAndQueuesView) {
    Action.async { request: Request[AnyContent] =>
      val startMillis = request.queryString.get("start").flatMap(_.headOption.map(_.toLong)).getOrElse(0L)
      val endMillis = request.queryString.get("end").flatMap(_.headOption.map(_.toLong)).getOrElse(0L)

      val futureState = futurePortState(Option(pointInTime), GetPortState(startMillis, endMillis))

      futureState
        .map { updates => Ok(write(updates)) }
        .recoverWith {
          case t =>
            log.error("Error processing request for port state", t)
            Future(InternalServerError)
        }
    }
  }

  def futurePortState(maybePointInTime: Option[MillisSinceEpoch], request: GetPortState): Future[Option[PortState]] = {
    val finalMessage = maybePointInTime match {
      case Some(pit) => PointInTimeQuery(pit, request)
      case _ => request
    }
    ctrl.portStateActor.ask(finalMessage).mapTo[Option[PortState]]
  }

  def futureUpdates(request: GetUpdatesSince): Future[Option[PortStateUpdates]] =
    ctrl.portStateActor.ask(request).mapTo[Option[PortStateUpdates]]

}
