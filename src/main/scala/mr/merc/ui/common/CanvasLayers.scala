package mr.merc.ui.common

import scalafx.scene.layout.Pane
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.control.ScrollBar
import scalafx.geometry.Orientation
import scalafx.geometry.Rectangle2D

// TODO handle case when CanvasLayer is resized - image shouldn't change position
class CanvasLayers(layers: List[CanvasLayer], fullMapSize: Rectangle2D) extends Pane with ScrollPaneLike {
  style = "-fx-background-color: black"
  val canvasList = layers map (l => (l, new Canvas()))
  val horBar = new ScrollBar
  horBar.orientation.value = Orientation.Horizontal
  horBar.max = 1
  horBar.min = 0
  val verBar = new ScrollBar
  verBar.orientation.value = Orientation.Vertical
  verBar.max = 1
  verBar.min = 0

  children.add(horBar)
  children.add(verBar)
  canvasList foreach (c => children.add(c._2))

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
    val hor = horBar.value.value
    val ver = verBar.value.value
    val horDiff = fullMapSize.width - canvasAreaWidth.value.doubleValue
    val verDiff = fullMapSize.height - canvasAreaHeight.value.doubleValue
    val x = hor * horDiff
    val y = ver * verDiff
    new Rectangle2D(x, y, canvasAreaWidth.value.doubleValue, canvasAreaHeight.value.doubleValue)
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

  verBar.prefWidth.value = 25
  horBar.prefHeight.value = 25

  private val canvasAreaWidth = this.width - verBar.prefWidth
  private val canvasAreaHeight = this.height - horBar.prefHeight
  verBar.prefHeight <== canvasAreaHeight
  horBar.prefWidth <== canvasAreaWidth
  verBar.visibleAmount <== canvasAreaHeight / fullMapSize.height
  horBar.visibleAmount <== canvasAreaWidth / fullMapSize.width
  verBar.unitIncrement = 0.1
  horBar.unitIncrement = 0.1

  def positionComponents() {
    canvasList.map(_._2).foreach { c =>
      c.layoutX = 0
      c.layoutY = 0
      c.width = canvasAreaWidth.value.doubleValue
      c.height = canvasAreaHeight.value.doubleValue
    }

    verBar.layoutX = canvasAreaWidth.value.doubleValue + 1
    verBar.layoutY = 0
    horBar.layoutX = 0
    horBar.layoutY = canvasAreaHeight.value.doubleValue + 1
  }

  this.width.onChange {
    positionComponents()
    redraw()
  }
  this.height.onChange {
    positionComponents()
    redraw()
  }

  val vvalue = verBar.value
  vvalue.onChange {
    redraw()
  }
  val hvalue = horBar.value
  hvalue.onChange {
    redraw()
  }
}

trait CanvasLayer {
  def updateLayer(gc: GraphicsContext, viewRect: Rectangle2D)
  def drawLayer(gc: GraphicsContext, viewRect: Rectangle2D)
}