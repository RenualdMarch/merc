package mr.merc.ui.world

import org.kordamp.ikonli.javafx.FontIcon
import scalafx.scene.control.Button
import scalafx.scene.layout.{Pane, Region}
import scalafx.Includes._

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

trait WorldInterfaceJavaNode {
  self: javafx.scene.layout.Region =>

  getStyleClass.add("interfacePane")
  getStylesheets.add("/css/worldPane.css")
}