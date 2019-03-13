package manifests.graph

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import drt.shared.SDateLike
import manifests.passengers.BestAvailableManifest
import manifests.{ManifestLookupLike, UniqueArrivalKey}
import org.specs2.mutable.SpecificationLike
import passengersplits.InMemoryPersistence

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class MockManifestLookupService(bestAvailableManifest: BestAvailableManifest) extends ManifestLookupLike {
  override def maybeBestAvailableManifest(arrivalPort: String, departurePort: String,
                                          voyageNumber: String, scheduled: SDateLike): Future[(UniqueArrivalKey, Option[BestAvailableManifest])] =
    Future((UniqueArrivalKey(arrivalPort, departurePort, voyageNumber, scheduled), Option(bestAvailableManifest)))
}

class ManifestGraphTestLike extends TestKit(ActorSystem("ManifestTests", InMemoryPersistence.akkaAndAggregateDbConfig))
  with SpecificationLike {
  isolated
  sequential

  implicit val actorSystem: ActorSystem = system
  implicit val materializer: ActorMaterializer = ActorMaterializer()

}
