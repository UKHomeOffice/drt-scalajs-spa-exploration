package drt.client.components.styles

import drt.client.components.styles.ScalaCssImplicits.CssSettings._
import scalacss.internal.mutable.StyleSheet

case class ArrivalsPageStyles(common: CommonStyle = DefaultCommonStyle) extends StyleSheet.Inline {

  import dsl._

  val redListCountryField: StyleA = style(
    borderBottom(solid, c"#ff9999", 3 px),
    paddingBottom(5 px),
    display.block
  )

}

object ArrivalsPageStylesDefault extends ArrivalsPageStyles
