package mr.merc.util

import mr.merc.ui.world.Components
import scalafx.scene.Node
import scalafx.scene.control.Label
import scalafx.Includes._
import javafx.scene.{input => jfxin}

object MercTooltip {

  private val label = new Label()
  label.stylesheets.add("/css/tooltip.css")
  label.styleClass.add("tooltip")
  label.style = s"-fx-font-size: ${Components.mediumFontSize}"
  label.setMouseTransparent(true)

  def applyTooltip(node: Node, text: String): Unit = {
    node.delegate.addEventHandler(jfxin.MouseEvent.MOUSE_ENTERED, (_: jfxin.MouseEvent) => {
      node.getScene.getChildren.remove(label)
      val bounds = node.localToScene(node.getBoundsInLocal)
      label.text = text
      label.layoutX = bounds.getCenterX
      label.layoutY = bounds.getCenterY

      node.getScene.getChildren.add(label)
    })

    node.delegate.addEventHandler(jfxin.MouseEvent.MOUSE_EXITED, (_: jfxin.MouseEvent) => {
      node.getScene.getChildren.remove(label)
    })

  }
}
