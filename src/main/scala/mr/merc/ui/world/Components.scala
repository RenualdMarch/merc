package mr.merc.ui.world

import mr.merc.log.Logging
import scalafx.beans.property.ReadOnlyStringProperty
import scalafx.scene.control.Button
import scalafx.scene.layout.{Pane, Region}
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

class PaneWithPropertyChild() extends Pane with Logging {

  def setNewChild(child: Region): Unit = {
    removeChild()
    addChild(child)
  }

  private def addChild(child: Region): Unit = {
    this.children.add(child)
    child.layoutX = 0
    child.layoutY = 0
    child.prefHeight <== this.height
    child.prefWidth <== this.width
  }

  private def removeChild(): Unit ={
    this.children.clear()
  }


}