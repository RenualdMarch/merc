package mr.merc.ui.world

import java.text.DecimalFormat

import mr.merc.local.Localization
import mr.merc.log.Logging
import org.tbee.javafx.scene.layout.MigPane
import scalafx.beans.binding.{ObjectBinding, StringBinding}
import scalafx.beans.property.{ObjectProperty, ReadOnlyObjectProperty, ReadOnlyStringProperty}
import scalafx.scene.{Node, Scene}
import scalafx.scene.layout.{Region}
import scalafx.scene.control.Button
import scalafx.scene.text.{Font, Text}
import scalafx.stage.Stage
import scalafx.Includes._
import mr.merc.util.FxPropertyUtils._

object Components {
  val largeFontSize = 24
  val mediumFontSize = 20
  val smallFontSize = 16

  val smallFontStyle = s"-fx-font-size: ${Components.smallFontSize}"
  val mediumFontStyle = s"-fx-font-size: ${Components.mediumFontSize}"
  val largeFontStyle = s"-fx-font-size: ${Components.largeFontSize}"
}

object BigText {
  def apply(t: String): BigText = new BigText {
    this.text = t
  }

  def apply(t: ReadOnlyObjectProperty[String]): BigText = new BigText {
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

  def apply(t: StringBinding): MediumText = new MediumText {
    this.text <== t
  }

  def apply(t: ObjectProperty[String]): MediumText = new MediumText {
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

  def apply(t: ReadOnlyStringProperty): BigButton = new BigButton {
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

  def apply(t: ReadOnlyStringProperty): MediumButton = new MediumButton {
    this.text <== t
  }
}

class MediumButton extends Button {
  this.font = Font(Components.mediumFontSize)
}

object IntFormatter {
  def apply(): DecimalFormat = {
    val asIntFormat = new DecimalFormat("#0")
    asIntFormat.setGroupingSize(3)
    asIntFormat.setGroupingUsed(true)
    asIntFormat
  }
}

object DoubleFormatter {
  def apply(): DecimalFormat = {
    val asIntFormat = new DecimalFormat("#0.00")
    asIntFormat.setGroupingSize(3)
    asIntFormat.setGroupingUsed(true)
    asIntFormat
  }
}

abstract class DialogStage[T] extends Stage with Logging {
  var dialogResult: Option[T] = None

  private val okButton = BigButton(Localization("common.ok"))
  okButton.onAction = { _ =>
    close()
  }

  private val cancelButton = BigButton(Localization("common.cancel"))
  cancelButton.onAction = { _ =>
    dialogResult = None
    close()
  }

  this.onCloseRequest = { _ =>
    dialogResult = None
  }

  private val buttonsPane = new MigPane()
  buttonsPane.add(okButton)
  buttonsPane.add(cancelButton)

  protected def dialogContent:Region

  protected def css:Option[String]

  private val actualDialogContent = dialogContent

  val contentPane = new MigPane("")
  contentPane.add(actualDialogContent, "grow, push, wrap")
  contentPane.add(buttonsPane, "center")

  val pane = new PaneForTooltip(contentPane)
  val currentScene = new Scene {
    css.foreach {c =>
      stylesheets.add(c)
    }
    content = pane
  }
  scene = currentScene
  actualDialogContent.width.onChange {
    Option(currentScene.window.value).foreach(_.sizeToScene())
  }
  actualDialogContent.height.onChange {
    Option(currentScene.window.value).foreach(_.sizeToScene())
  }
}