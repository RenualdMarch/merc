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
  self: javafx.scene.layout.Region =>

  getStyleClass.add("whitePane")
  getStylesheets.add("/css/worldPane.css")
}

object PaneWithTwoEqualHorizontalChildren {

  def apply(first: Region, second: Region): PaneWithTwoEqualHorizontalChildren = {
    val p = new PaneWithTwoEqualHorizontalChildren()
    p.setTwoChildren(first, second)
    p
  }
}

class PaneWithTwoEqualHorizontalChildren extends Pane {

  def setTwoChildren(first: Region, second: Region): Unit = {
    this.children.clear()

    first.layoutX = 0
    first.layoutY = 0
    first.prefWidth <== this.width / 2
    first.prefHeight <== this.height

    second.layoutX <== this.width / 2
    second.layoutY = 0
    second.prefWidth <== this.width / 2
    second.prefHeight <== this.height

    this.children.addAll(first, second)
  }
}

object PaneWithTwoEqualVerticalChildren {

  def apply(first: Region, second: Region): PaneWithTwoEqualVerticalChildren = {
    val p = new PaneWithTwoEqualVerticalChildren()
    p.setTwoChildren(first, second)
    p
  }
}

class PaneWithTwoEqualVerticalChildren extends Pane {

  def setTwoChildren(first: Region, second: Region): Unit = {
    this.children.clear()

    first.layoutX = 0
    first.layoutY = 0
    first.prefWidth <== this.width
    first.prefHeight <== this.height / 2

    second.layoutX = 0
    second.layoutY <== this.height / 2
    second.prefWidth <== this.width
    second.prefHeight <== this.height / 2

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