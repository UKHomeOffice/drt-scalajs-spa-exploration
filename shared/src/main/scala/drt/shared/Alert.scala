package drt.shared

import drt.shared.CrunchApi.MillisSinceEpoch
import upickle.default.{macroRW, ReadWriter => RW}

case class Alert(title: String, message: String, alertClass: String, expires: MillisSinceEpoch, createdAt: MillisSinceEpoch)

object Alert {
  implicit val rw: RW[Alert] = macroRW
}
