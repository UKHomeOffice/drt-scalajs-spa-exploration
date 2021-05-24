package controllers.application

import controllers.Application
import drt.shared.FeatureFlags
import play.api.mvc.{Action, AnyContent}


trait WithFeatureFlags {
  self: Application =>

  def getFeatureFlags: Action[AnyContent] = Action { _ =>
    import upickle.default._

    val frontendFeatures = FeatureFlags(
      useApiPaxNos = ctrl.params.useApiPaxNos,
      displayWaitTimesToggle = ctrl.params.enableToggleDisplayWaitTimes,
      displayRedListInfo = ctrl.params.displayRedListInfo)

    Ok(write(frontendFeatures))
  }
}
