package mr.merc.ui.world

import scalafx.beans.property.ReadOnlyStringProperty
import scalafx.scene.control.Button
import scalafx.scene.text.{Font, Text}

object Components {
  val largeFontSize = 24
  val smallFontSize = 20
}

object BigText {
  def apply(t: String): BigText = new BigText {
    this.text = t
  }

  def apply(t:ReadOnlyStringProperty): BigText = new BigText {
    this.text <== t
  }
}

class BigText extends Text {
  this.font = Font(Components.largeFontSize)
}

object SmallText {
  def apply(t: String): SmallText = new SmallText {
    this.text = t
  }

  def apply(t:ReadOnlyStringProperty): SmallText = new SmallText {
    this.text <== t
  }
}

class SmallText extends Text {
  this.font = Font(Components.smallFontSize)
}

object BigButton {
  def apply(t: String): BigButton = new BigButton {
    this.text = t
  }

  def apply(t:ReadOnlyStringProperty): BigButton = new BigButton {
    this.text <== t
  }
}

class BigButton extends Button {
  this.font = Font(Components.largeFontSize)
}

object SmallButton {
  def apply(t: String): SmallButton = new SmallButton {
    this.text = t
  }

  def apply(t:ReadOnlyStringProperty): SmallButton = new SmallButton {
    this.text <== t
  }
}

class SmallButton extends Button {
  this.font = Font(Components.smallFontSize)
}