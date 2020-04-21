package controllers.model

import akka.actor.ActorRef
import akka.pattern.ask
import drt.shared.CrunchApi.PortStateError
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object ActorDataRequest {
  val log: Logger = LoggerFactory.getLogger(getClass)

  def portState[X](actorRef: ActorRef, message: Any)(implicit ec: ExecutionContext): Future[Either[PortStateError, Option[X]]] = {
    actorRef
      .ask(message)(30 seconds)
      .map {
        case Some(ps: X) => Right(Option(ps))
        case _ => Right(None)
      }
      .recover {
        case t => Left(PortStateError(t.getMessage))
      }
  }
}
