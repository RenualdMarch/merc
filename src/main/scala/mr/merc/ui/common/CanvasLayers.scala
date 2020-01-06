package mr.merc.ui.common

import scalafx.beans.property.{DoubleProperty, ObjectProperty, ReadOnlyDoubleProperty, ReadOnlyObjectProperty}
import scalafx.scene.layout.Pane
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.canvas.Canvas
import scalafx.geometry.Rectangle2D
import mr.merc.util.FxPropertyUtils._

// TODO handle case when CanvasLayer is resized - image shouldn't change position
// TODO update canvas count in case layers count change
class CanvasLayers(private var _layers: List[CanvasLayer], fullMapSize: Rectangle2D) extends Pane with ScrollPaneLike {
  style = "-fx-background-color: black"

  def layers:List[CanvasLayer] = _layers

  def layers_=(layers:List[CanvasLayer]):Unit = {
    if (layers.size != _layers.size) {
      this.children.clear()
      _layers = layers
      canvasList = initCanvasLists()
    } else {
      _layers = layers
      canvasList = canvasList.zip(layers).map { case ((_, c), cl) => (cl, c)}
    }
  }

  private var canvasList = initCanvasLists()

  def initCanvasLists():List[(CanvasLayer, Canvas)] = {
    val result = layers map (l => (l, new Canvas()))
    result foreach { case (_, c) =>
      c.width <== this.width
      c.height <== this.height
      children.add(c)
    }
    result
  }

  initCanvasLists()

  def redraw() {
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