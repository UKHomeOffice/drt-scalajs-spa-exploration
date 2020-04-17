package test.feeds.test

import actors.SubscribeResponseQueue
import akka.actor.{Actor, ActorLogging, Scheduler}
import akka.stream.scaladsl.SourceQueueWithComplete
import passengersplits.parsing.VoyageManifestParser.{VoyageManifest, VoyageManifests}
import server.feeds.{DqManifests, ManifestsFeedResponse, ManifestsFeedSuccess}
import services.OfferHandler
import test.TestActors.ResetData

import scala.concurrent.ExecutionContext.Implicits.global


case object GetManifests

class TestManifestsActor extends Actor with ActorLogging {

  implicit val scheduler: Scheduler = this.context.system.scheduler

  var maybeManifests: Option[Set[VoyageManifest]] = None
  var maybeSubscriber: Option[SourceQueueWithComplete[ManifestsFeedResponse]] = None

  override def receive: PartialFunction[Any, Unit] = {
    case VoyageManifests(manifests) =>
      log.info(s"Got these VMS: ${manifests.map{m => s"${m.EventCode} ${m.CarrierCode}${m.flightCode}"}}")

      maybeSubscriber match {
        case Some(subscriber) =>
          OfferHandler.offerWithRetries(subscriber, ManifestsFeedSuccess(DqManifests("", manifests)), 5)
          maybeManifests = None
        case None =>
          maybeManifests = Some(manifests)
      }


    case ResetData =>
      maybeManifests = None

    case SubscribeResponseQueue(manifestsResponse) =>
      maybeSubscriber = Option(manifestsResponse)
      maybeManifests.foreach(manifests => ManifestsFeedSuccess(DqManifests("", manifests)))
  }
}
