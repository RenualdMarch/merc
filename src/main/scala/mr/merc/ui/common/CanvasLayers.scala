package mr.merc.ui.common

import mr.merc.util.MercUtils
import scalafx.beans.property.DoubleProperty
import scalafx.scene.layout.Pane
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.canvas.Canvas
import scalafx.geometry.Rectangle2D

// TODO handle case when CanvasLayer is resized - image shouldn't change position
class CanvasLayers(layers: List[CanvasLayer], fullMapSize: Rectangle2D) extends Pane with ScrollPaneLike {
  style = "-fx-background-color: black"
  val canvasList = layers map (l => (l, new Canvas()))

  canvasList foreach { case (_, c) =>
    c.width <== this.width
    c.height <== this.height
    children.add(c)
  }

  private def redraw() {
    canvasList foreach {
      case (layer, canvas) =>
        if (canvas.width.value >= 0 && canvas.height.value >= 0) {
          val gc = canvas.graphicsContext2D
          layer.drawLayer(gc, viewRect)
        }
    }
  }

  def viewRect: Rectangle2D = {
    val horDiff = fullMapSize.width - this.width.value
    val verDiff = fullMapSize.height - this.height.value
    val x = hvalue.value * horDiff
    val y = vvalue.value * verDiff
    new Rectangle2D(x, y, this.width.value, this.height.value)
  }

  def updateCanvas() {
    canvasList.foreach {
      case (layer, canvas) =>
        if (canvas.width.value >= 0 && canvas.height.value >= 0) {
          val gc = canvas.graphicsContext2D
          layer.updateLayer(gc, viewRect)
        }
    }
  }

  private val canvasAreaWidth = this.width
  private val canvasAreaHeight = this.height


  def positionComponents() {
    canvasList.map(_._2).foreach { c =>
      c.layoutX = 0
      c.layoutY = 0
    }
  }

  this.width.onChange {
    positionComponents()
    redraw()
  }
  this.height.onChange {
    positionComponents()
    redraw()
  }

  val vvalue = DoubleProperty(0)
  vvalue.onChange {
    redraw()
  }
  val hvalue = DoubleProperty(0)
  hvalue.onChange {
    redraw()
  }
}

trait CanvasLayer {
  def updateLayer(gc: GraphicsContext, viewRect: Rectangle2D)

  def drawLayer(gc: GraphicsContext, viewRect: Rectangle2D)
}