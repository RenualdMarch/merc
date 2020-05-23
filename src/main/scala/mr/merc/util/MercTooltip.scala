package mr.merc.util

import mr.merc.ui.world.Components
import scalafx.scene.Node
import scalafx.scene.control.Label
import scalafx.Includes._
import javafx.scene.{input => jfxin}
import mr.merc.util.MercTooltip.TooltipRecalculateWithCoords
import scalafx.geometry.Point2D
import scalafx.scene.effect.Light.Point
import scalafx.scene.layout.BorderPane

object MercTooltip {

  private val tooltipContainer: BorderPane = new BorderPane()
  tooltipContainer.setMouseTransparent(true)

  private def buildLabel(f:(Double, Double) => Option[String]):Node with TooltipRecalculateWithCoords= {
    val label = new Label with TooltipRecalculateWithCoords {
      override def recalculateTooltip(x: Double, y: Double): Boolean = {
        val s = f(x, y)
        this.text = s.getOrElse("")
        s.isDefined
      }
    }
    label.stylesheets.add("/css/tooltip.css")
    label.styleClass.add("tooltip")
    label.style = Components.mediumFontStyle
    label
  }

  private def buildLabel(text: String):Node = {
    val label = new Label
    label.text = text
    label.stylesheets.add("/css/tooltip.css")
    label.styleClass.add("tooltip")
    label.style = Components.mediumFontStyle
    label
  }

  def applyCenterTooltip(node: Node, tooltipNode: Node): Unit = {
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

  def applyTooltip(node: Node, tooltipNode: Node with TooltipRecalculateWithCoords): Unit = {
    tooltipNode.setMouseTransparent(true)
    node.delegate.addEventHandler(jfxin.MouseEvent.MOUSE_MOVED, (me: jfxin.MouseEvent) => {
      node.getScene.getChildren.remove(tooltipContainer)
      val bounds = node.sceneToLocal(new Point2D(me.getSceneX, me.getSceneY))
      val show = tooltipNode.recalculateTooltip(bounds.getX, bounds.getY)
      if (show) {
        tooltipContainer.center = tooltipNode
        tooltipContainer.layoutX = me.getSceneX
        tooltipContainer.layoutY = me.getSceneY
        node.getScene.getChildren.add(tooltipContainer)
      }
    })

    node.delegate.addEventHandler(jfxin.MouseEvent.MOUSE_EXITED, (_: jfxin.MouseEvent) => {
      node.getScene.getChildren.remove(tooltipContainer)
    })
  }

  def applyTooltip(node: Node, tooltipNode: javafx.scene.Node with TooltipRecalculateWithCoords): Unit = {
    tooltipNode.setMouseTransparent(true)
    node.addEventHandler(jfxin.MouseEvent.MOUSE_MOVED, (me: jfxin.MouseEvent) => {
      node.getScene.getChildren.remove(tooltipContainer)
      val bounds = node.sceneToLocal(new Point2D(me.getSceneX, me.getSceneY))
      val show = tooltipNode.recalculateTooltip(bounds.getX, bounds.getY)
      if(show) {
        tooltipContainer.center = tooltipNode
        tooltipContainer.layoutX = me.getSceneX
        tooltipContainer.layoutY = me.getSceneY
        node.getScene.getChildren.add(tooltipContainer)
      }
    })

    node.addEventHandler(jfxin.MouseEvent.MOUSE_EXITED, (_: jfxin.MouseEvent) => {
      node.getScene.getChildren.remove(tooltipContainer)
    })
  }

  def applyTooltip(node: Node, text: String): Unit = {
    applyCenterTooltip(node, buildLabel(text))
  }

  def applyTooltip(node: Node, f:(Double, Double) => Option[String]): Unit = {
    applyTooltip(node, buildLabel(f))
  }

  trait TooltipRecalculateWithCoords {
    def recalculateTooltip(x:Double, y:Double): Boolean
  }
}
