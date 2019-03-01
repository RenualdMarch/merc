package mr.merc.ui.world

import java.text.DecimalFormat

import scalafx.beans.property.ReadOnlyStringProperty
import scalafx.scene.control.Button
import scalafx.scene.text.{Font, Text}

object Components {
  val largeFontSize = 24
  val mediumFontSize = 20
  val smallFontSize = 16
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

object MediumText {
  def apply(t: String): MediumText = new MediumText {
    this.text = t
  }

  def apply(t:ReadOnlyStringProperty): MediumText = new MediumText {
    this.text <== t
  }
}

class MediumText extends Text {
  this.font = Font(Components.mediumFontSize)
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

object MediumButton {
  def apply(t: String): MediumButton = new MediumButton {
    this.text = t
  }

  def apply(t:ReadOnlyStringProperty): MediumButton = new MediumButton {
    this.text <== t
  }
}

class MediumButton extends Button {
  this.font = Font(Components.mediumFontSize)
}

object IntFormatter {
  def apply():DecimalFormat = {
    val asIntFormat = new DecimalFormat("#0")
    asIntFormat.setGroupingSize(3)
    asIntFormat.setGroupingUsed(true)
    asIntFormat
  }
}

object DoubleFormatter {
  def apply():DecimalFormat = {
    val asIntFormat = new DecimalFormat("#0.00")
    asIntFormat.setGroupingSize(3)
    asIntFormat.setGroupingUsed(true)
    asIntFormat
  }
}