package mr.merc.ui.world

import java.text.DecimalFormat

import javafx.beans.value.WritableValue
import mr.merc.local.Localization
import mr.merc.log.Logging
import org.tbee.javafx.scene.layout.MigPane
import scalafx.beans.binding.{ObjectBinding, StringBinding}
import scalafx.beans.property.{ObjectProperty, ReadOnlyObjectProperty, ReadOnlyStringProperty, StringProperty}
import scalafx.scene.{Node, Scene}
import scalafx.scene.layout.{BorderPane, ColumnConstraints, GridPane, Region}
import scalafx.scene.control.{Button, Spinner, TableCell, TableColumn}
import scalafx.scene.text.{Font, Text}
import scalafx.stage.Stage
import scalafx.Includes._
import mr.merc.util.FxPropertyUtils._

object Components {
  val largeFontSize = 24
  val mediumFontSize = 20
  val smallFontSize = 16

  val smallFontStyle = s"-fx-font-size: ${Components.smallFontSize};"
  val mediumFontStyle = s"-fx-font-size: ${Components.mediumFontSize};"
  val largeFontStyle = s"-fx-font-size: ${Components.largeFontSize};"
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
  val dialogResultProperty: ObjectProperty[Option[T]] = ObjectProperty(None)

  def dialogResult = dialogResultProperty.value
  def dialogResult_=(t:Option[T]): Unit = {
    dialogResultProperty.value = t
  }

  protected def onOkButtonPressed(): Unit = {

  }

  private val okButton = BigButton(Localization("common.ok"))
  okButton.onAction = { _ =>
    onOkButtonPressed()
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

  protected def additionalButtons:List[Button] = Nil

  private val buttonsPane = new MigPane()
  buttonsPane.add(okButton)
  additionalButtons.foreach(buttonsPane.add(_))
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

class StringColumn[T](title: String, f: T => String) extends TableColumn[T, String] {
  text = title
  cellValueFactory = p => StringProperty(f(p.value))
  editable = false
  style = Components.largeFontStyle + "-fx-alignment: center-right;"
}

object GridPaneBuilder {

  def buildWithoutCaption(constraints:List[Double], nodes:List[Node]): GridPane = {
    val cc = constraints.map { c =>
      new ColumnConstraints {
        percentWidth = c
      }
    }

    new GridPane {
      columnConstraints = cc
      nodes.grouped(constraints.size).zipWithIndex.foreach { case (row, r) =>
        row.zipWithIndex.foreach { case (el, col) =>
          add(el, col, r)
        }
      }
    }
  }

  def buildWithCaption(constraints: List[Double], nodes: List[Node], captions:List[Node]): GridPane = {
    buildWithoutCaption(constraints, captions ++ nodes)
  }

  def buildWithCaptionString(constraints:List[Double], nodes:List[String], captions:List[String]): GridPane = {
    def mediumText(label:String): Node = new BorderPane {
      left = MediumText(label)
      style = "-fx-border-color: black;-fx-border-width: 1 1 1 1; -fx-padding: 10 10 10 10;"
    }
    buildWithCaption(constraints, nodes.map(mediumText), captions.map(mediumText))
  }
}

class IntSpinnerCell[T] extends javafx.scene.control.TableCell[T, Int] {
  private val spinner = new Spinner[Int](0, Int.MaxValue, 0, 1)
  private var ignoreUpdate = false // flag preventing updates triggered from ui/initialisation

  spinner.value.onChange { (o, oldValue, newValue) =>
    if (!ignoreUpdate) {
      ignoreUpdate = true
      val property = getTableColumn.getCellObservableValue(getTableRow.getIndex).asInstanceOf[WritableValue[Int]]
      property.setValue(newValue)
      ignoreUpdate = false
    }
  }

  override def updateItem(item:Int, empty:Boolean): Unit = {
    super.updateItem(item, empty)
    if (empty) {
      setGraphic(null)
    } else {
      ignoreUpdate = true
      spinner.valueFactory.value.setValue(item)
      setGraphic(spinner)
      ignoreUpdate = false
    }
  }
}