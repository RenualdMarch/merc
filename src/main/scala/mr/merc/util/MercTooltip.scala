package mr.merc.util

import mr.merc.ui.world.Components
import scalafx.scene.Node
import scalafx.scene.control.Label
import scalafx.Includes._
import javafx.scene.{input => jfxin}
import scalafx.scene.layout.BorderPane

object MercTooltip {

  private val tooltipContainer: BorderPane = new BorderPane()
  tooltipContainer.setMouseTransparent(true)

  private def buildLabel(text: String):Node = {
    val label = new Label()
    label.stylesheets.add("/css/tooltip.css")
    label.styleClass.add("tooltip")
    label.style = Components.mediumFontStyle
    label.text = text
    label
  }

  def applyTooltip(node: Node, tooltipNode: Node): Unit = {
    tooltipNode.setMouseTransparent(true)
    node.delegate.addEventHandler(jfxin.MouseEvent.MOUSE_ENTERED, (_: jfxin.MouseEvent) => {
      node.getScene.getChildren.remove(tooltipContainer)
      val bounds = node.localToScene(node.getBoundsInLocal)
      tooltipContainer.center = tooltipNode
      tooltipContainer.layoutX = bounds.getCenterX
      tooltipContainer.layoutY = bounds.getCenterY
      node.getScene.getChildren.add(tooltipContainer)
    })

    node.delegate.addEventHandler(jfxin.MouseEvent.MOUSE_EXITED, (_: jfxin.MouseEvent) => {
      node.getScene.getChildren.remove(tooltipContainer)
    })
  }

  def applyTooltip(node: Node, text: String): Unit = {
    applyTooltip(node, buildLabel(text))
  }

}
