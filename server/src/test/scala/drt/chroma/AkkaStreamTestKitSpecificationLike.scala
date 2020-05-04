package drt.chroma

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.testkit.TestSubscriber.Probe
import akka.testkit.TestKit
import com.typesafe.config.ConfigFactory
import org.specs2.execute.Result
import org.specs2.mutable.SpecificationLike

import scala.reflect.macros.blackbox

abstract class AkkaStreamTestKitSpecificationLike extends
  TestKit(ActorSystem("AkkaStreamTestKitSpecificationLike", ConfigFactory.empty())) with SpecificationLike {
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  implicit def probe2Success[R <: Probe[_]](r: R): Result = success
}

