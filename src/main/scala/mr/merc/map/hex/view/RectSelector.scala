package mr.merc.map.hex.view

import scalafx.geometry.Rectangle2D
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import Math._

class RectSelector(terrainHexFieldView: TerrainHexFieldView) {

  private var currentlyDrawn: Option[Rectangle2D] = None
  private var shouldBeDrawn: Option[Rectangle2D] = None

  private var initialPoint: Option[(Double, Double)] = None

  def onMouseDown(x: Double, y: Double): Unit = {
    initialPoint = Some((x, y))
  }

  def onMouseMove(x: Double, y: Double): Unit = {
    initialPoint.foreach { case (startX, startY) =>
      val minX = min(x, startX)
      val minY = min(y, startY)
      val width = abs(x - startX)
      val height = abs(y - startY)
      shouldBeDrawn = Some(new Rectangle2D(minX, minY, width, height))
    }
  }

  def onMouseUp(): Option[List[TerrainHexView]] = {
    val result = shouldBeDrawn.map { rect =>
      terrainHexFieldView.selectedHexes(rect)
    }
    clear()
    result
  }

  def onMouseLeft(): Unit = {
    clear()
  }

  def clear(): Unit = {
    initialPoint = None
    shouldBeDrawn = None
  }

  def drawSelectionIfNeeded(gc: GraphicsContext, viewRect: Rectangle2D): Unit = {
    (currentlyDrawn, shouldBeDrawn) match {
      case (None, None) => // do nothing
      case (Some(rect), None) => clearRectangle(gc, rect, viewRect)
      case (None, Some(rect)) => drawRectangle(gc, rect, viewRect)
      case (Some(prev), Some(next)) => if (prev != next) {
        clearRectangle(gc, prev, viewRect)
        drawRectangle(gc, next, viewRect)
      }
    }
    currentlyDrawn = shouldBeDrawn
  }

  private def clearRectangle(gc: GraphicsContext, rect: Rectangle2D, viewRect: Rectangle2D): Unit = {
    val d = 3
    val x = rect.minX - viewRect.minX
    val y = rect.minY - viewRect.minY
    gc.clearRect(x - d, y - d, rect.width + 2 * d, rect.height + 2 * d)
  }

  private def drawRectangle(gc: GraphicsContext, rect: Rectangle2D, viewRect: Rectangle2D): Unit = {
    gc.save()
    gc.stroke = Color.Yellow
    gc.lineWidth = 2
    gc.strokeRect(rect.minX - viewRect.minX, rect.minY - viewRect.minY, rect.width, rect.height)
    gc.restore()
  }
}
