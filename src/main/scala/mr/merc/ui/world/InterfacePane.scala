package mr.merc.ui.world

import org.kordamp.ikonli.javafx.FontIcon
import scalafx.scene.control.Button
import scalafx.scene.layout.{BorderPane, Pane, Region}
import scalafx.Includes._
import scalafx.beans.property.ReadOnlyObjectProperty
import scalafx.geometry.Pos
import scalafx.scene.Node

class InterfacePane(content: Region, onClose: () => Unit) extends Pane with WorldInterfaceNode {
  private val iconSize = 32
  private val iconPadding = 2
  private val closeButton = new Button()
  closeButton.onMouseClicked = _ => onClose()
  private val icon = new FontIcon("enty-squared-cross")
  icon.setIconSize(iconSize)
  closeButton.graphic = icon
  closeButton.style = s"-fx-padding: $iconPadding;"

  closeButton.layoutX <== this.width - iconSize - iconPadding * 2
  closeButton.layoutY = 0
  closeButton.prefWidth = iconSize
  closeButton.prefHeight = iconSize

  content.layoutX = 0
  content.layoutY = 0
  content.prefWidth <== this.width
  content.prefHeight <== this.height

  this.children = List(content, closeButton)
}


trait WorldInterfaceNode {
  self: Region =>

  styleClass.add("interfacePane")
  stylesheets.add("/css/worldPane.css")
}

trait WorldInterfaceWhiteNode {
  self: Region =>

  styleClass.add("whitePane")
  stylesheets.add("/css/worldPane.css")
}


trait WorldInterfaceJavaNode {
  self: javafx.scene.layout.Region =>

  getStyleClass.add("interfacePane")
  getStylesheets.add("/css/worldPane.css")
}

trait WorldInterfaceWhiteJavaNode {
  self: javafx.scene.Parent =>

  getStyleClass.add("whitePane")
  getStylesheets.add("/css/worldPane.css")
}

object PaneWithTwoHorizontalChildren {

  def apply(first: Region, second: Region, left:Double = 0.5): PaneWithTwoHorizontalChildren = {
    val p = new PaneWithTwoHorizontalChildren(left)
    p.setTwoChildren(first, second)
    p
  }
}

class PaneWithTwoHorizontalChildren(left:Double = 0.5) extends Pane {

  def setTwoChildren(first: Region, second: Region): Unit = {
    this.children.clear()

    first.layoutX = 0
    first.layoutY = 0
    first.prefWidth <== this.width * left
    first.prefHeight <== this.height

    second.layoutX <== this.width * left
    second.layoutY = 0
    second.prefWidth <== this.width * (1 - left)
    second.prefHeight <== this.height

    this.children.addAll(first, second)
  }
}

object PaneWithTwoVerticalChildren {

  def apply(first: Region, second: Region, upper:Double = 0.5): PaneWithTwoVerticalChildren = {
    val p = new PaneWithTwoVerticalChildren(upper)
    p.setTwoChildren(first, second)
    p
  }
}

class PaneWithTwoVerticalChildren(upper:Double = 0.5) extends Pane {

  def setTwoChildren(first: Region, second: Region): Unit = {
    this.children.clear()

    first.layoutX = 0
    first.layoutY = 0
    first.prefWidth <== this.width
    first.prefHeight <== this.height * upper

    second.layoutX = 0
    second.layoutY <== this.height * upper
    second.prefWidth <== this.width
    second.prefHeight <== this.height * (1 - upper)

    this.children.addAll(first, second)
  }
}

class PropertyDependentPane[T](property: ReadOnlyObjectProperty[T], f:T => Node) extends BorderPane {

  def reload(): Unit = {
    center = f(property.value)
  }

  property.onChange {
    reload()
  }

  reload()
}

class TopTitledBorderPane extends BorderPane {
  top.onChange {
    BorderPane.setAlignment(top.value, Pos.TopCenter)
  }
}

class PaneForTooltip(content:Region) extends Pane {

  this.prefWidth <== content.prefWidth
  this.prefHeight <== content.prefHeight

  children.add(content)
}