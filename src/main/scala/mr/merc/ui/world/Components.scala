package mr.merc.ui.world

import scalafx.scene.control.Button
import scalafx.scene.text.{Font, Text}

object Components {
  val largeSize = 24
  val mediumSize = 20
  val smallSize = 16
}

case class BigText(t: String) extends Text(t) {
  this.font = Font(Components.largeSize)
}

case class MediumText(t: String) extends Text(t) {
  this.font = Font(Components.mediumSize)
}

case class SmallText(t: String) extends Text(t) {
  this.font = Font(Components.smallSize)
}

case class BigButton(t: String) extends Button(t) {
  this.font = Font(Components.largeSize)
}

case class MediumButton(t: String) extends Button(t) {
  this.font = Font(Components.mediumSize)
}

class SmallButton(t: String) extends Button(t) {
  this.font = Font(Components.smallSize)
}