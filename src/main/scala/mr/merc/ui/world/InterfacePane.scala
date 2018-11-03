package mr.merc.ui.world

import org.kordamp.ikonli.javafx.FontIcon
import scalafx.scene.Node
import scalafx.scene.control.Button
import scalafx.scene.layout.{BorderPane, HBox, Region}
import scalafx.Includes._

class InterfacePane(content: Node, onClose: () => Unit) extends BorderPane with WorldInterfaceNode {
  this.top = new InterfacePaneTop(onClose)
  this.center = content
}

class InterfacePaneTop(onClose: () => Unit) extends BorderPane {
  private val closeButton = new Button()
  closeButton.onMouseClicked = _ => onClose()
  private val icon = new FontIcon("enty-squared-cross")
  icon.setIconSize(24)
  closeButton.graphic = icon

  this.right = closeButton
}

trait WorldInterfaceNode {
  self: Region =>

  styleClass.add("interfacePane")
  stylesheets.add("/css/worldPane.css")
}