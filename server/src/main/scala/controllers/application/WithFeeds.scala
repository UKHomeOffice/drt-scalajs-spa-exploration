package controllers.application

import java.util.UUID

import actors.GetState
import actors.pointInTime.ArrivalsReadActor
import akka.actor.{ActorRef, PoisonPill}
import akka.pattern.AskableActorRef
import controllers.Application
import drt.shared.CrunchApi.MillisSinceEpoch
import drt.shared._
import play.api.mvc.{Action, AnyContent}
import services.SDate
import upickle.default.write

import scala.concurrent.Future


trait WithFeeds {
  self: Application =>

  def getFeedStatuses: Action[AnyContent] = auth {
    Action.async { _ =>
      ctrl.getFeedStatus.map((s: Seq[FeedSourceStatuses]) => {
        val safeStatusMessages = s
          .map(feedSourceStatuses => feedSourceStatuses
            .copy(feedStatuses = feedSourceStatuses
              .feedStatuses
              .copy(feedSourceStatuses.feedStatuses.statuses.map {
                case f: FeedStatusFailure =>
                  f.copy(message = "Unable to connect to feed.")
                case s => s
              })))
        Ok(write(safeStatusMessages))
      })
    }
  }

  def getArrival(number: Int, terminal: String, scheduled: MillisSinceEpoch): Action[AnyContent] = authByRole(ArrivalSource) {
    Action.async { _ =>
      val futureArrivalSources = ctrl.feedActors
        .map(feed =>
          feed
            .ask(UniqueArrival(number, terminal, scheduled))
            .map {
              case Some(fsa: FeedSourceArrival) if ctrl.isValidFeedSource(fsa.feedSource) => Option(fsa)
              case _ => None
            }
        )

      Future
        .sequence(futureArrivalSources)
        .map(arrivalSources => Ok(write(arrivalSources.filter(_.isDefined))))
    }
  }

  def getArrivalAtPointInTime(
                               pointInTime: MillisSinceEpoch,
                               number: Int, terminal: String,
                               scheduled: MillisSinceEpoch
                             ): Action[AnyContent] = authByRole(ArrivalSource) {
    val arrivalActorPersistenceIds = Seq(
      "actors.LiveBaseArrivalsActor-live-base",
      "actors.LiveArrivalsActor-live",
      "actors.ForecastBaseArrivalsActor-forecast-base",
      "actors.ForecastPortArrivalsActor-forecast-port"
    )

    val pointInTimeActorSources: Seq[ActorRef] = arrivalActorPersistenceIds.map(id => system.actorOf(
      ArrivalsReadActor.props(SDate(pointInTime), id),
      name = s"arrival-read-$id-${UUID.randomUUID()}"
    ))
    Action.async { _ =>

      val futureArrivalSources = pointInTimeActorSources.map((feedActor: ActorRef) => {

        val askableActorRef: AskableActorRef = feedActor

        askableActorRef
          .ask(UniqueArrival(number, terminal, scheduled))
          .map {
            case Some(fsa: FeedSourceArrival) =>
              feedActor ! PoisonPill
              Option(fsa)
            case _ =>
              feedActor ! PoisonPill
              None
          }
      })
      Future
        .sequence(futureArrivalSources)
        .map(arrivalSources => Ok(write(arrivalSources.filter(_.isDefined))))
    }
  }
}
