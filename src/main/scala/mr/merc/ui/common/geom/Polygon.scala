package mr.merc.ui.common.geom

import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color

class Polygon(points: (Double, Double)*) {

  private val fxPolygon = new scalafx.scene.shape.Polygon
  private val pointsArr = points.flatMap { case (x, y) => List(x, y) }
  import collection.JavaConverters._
  fxPolygon.points.setAll(pointsArr.map(double2Double).asJava)

  def drawPolygon(xOffset: Int, yOffset: Int, gc: GraphicsContext, color: Color) {
    gc.save()
    gc.fill = color
    val offsetPoints = points map { case (x, y) => (xOffset + x, yOffset + y) }
    gc.fillPolygon(offsetPoints)
    gc.restore()
  }

  def isInside(x: Double, y: Double) = fxPolygon.contains(x, y)

}