package mr.merc.ui.common

import scalafx.scene.layout.Pane
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.control.ScrollBar
import scalafx.geometry.Orientation
import scalafx.geometry.Rectangle2D

// TODO handle case when CanvasLayer is resized - image shouldn't change position
class CanvasLayer(val layersCount: Int, fullMapSize: Rectangle2D, cleanRedraw: (Int, Rectangle2D, GraphicsContext) => Unit) extends Pane with ScrollPaneLike {
  val canvasArray = 0 until layersCount map (i => new Canvas()) toVector
  val horBar = new ScrollBar
  horBar.orientation.value = Orientation.HORIZONTAL
  horBar.max = 1
  horBar.min = 0
  horBar.blockIncrement = 0.1
  val verBar = new ScrollBar
  verBar.orientation.value = Orientation.VERTICAL
  verBar.max = 1
  verBar.min = 0
  verBar.blockIncrement = 0.1

  children.add(horBar)
  children.add(verBar)
  canvasArray foreach (children.add(_))

  private def redraw() {
    0 until layersCount foreach { i =>
      val canvas = canvasArray(i)
      val gc = canvas.graphicsContext2D
      gc.clearRect(0, 0, canvas.width.value, canvas.height.value)
      cleanRedraw(i, viewRect, canvasArray(i).graphicsContext2D)
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

  def updateCanvas(layer: Int)(f: (GraphicsContext, Rectangle2D) => Unit) {
    val gc = canvasArray(layer).graphicsContext2D
    f(gc, viewRect)
  }

  private val canvasAreaWidth = this.width - verBar.prefWidth
  private val canvasAreaHeight = this.height - horBar.prefHeight

  def positionComponents() {
    canvasArray.foreach { c =>
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
  val hvalue = horBar.value
}