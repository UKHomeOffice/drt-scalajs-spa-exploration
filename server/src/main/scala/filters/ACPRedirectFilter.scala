package filters

import akka.stream.Materializer
import javax.inject.{Inject, Singleton}
import org.slf4j.{Logger, LoggerFactory}
import play.api.Configuration
import play.api.mvc.{Filter, RequestHeader, Result, Results}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ACPRedirectFilter @Inject()(
                                   implicit val config: Configuration,
                                   implicit override val mat: Materializer,
                                   exec: ExecutionContext) extends Filter {
  val log: Logger = LoggerFactory.getLogger(getClass)
  val acpRedirectFlagIsSet: Boolean = config.get[Boolean]("feature-flags.acp-redirect")
  val portCode: String = config.get[String]("portcode")
  val redirectUrl = s"https://$portCode.drt.homeoffice.gov.uk/v2/$portCode/live"

  override def apply(next: RequestHeader => Future[Result])
                    (requestHeader: RequestHeader): Future[Result] = {
    if (acpRedirectFlagIsSet) {
      log.info(s"ACPRedirectFilter: Redirecting to $redirectUrl")
      Future(Results.Redirect(redirectUrl))
    } else {
      next(requestHeader)
    }
  }
}