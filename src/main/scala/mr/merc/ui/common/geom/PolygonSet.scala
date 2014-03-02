package mr.merc.ui.common.geom

import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color

class PolygonSet(polygons: Polygon*) {

  def drawPolygons(xOffset: Int, yOffset: Int, gc: GraphicsContext, color: Color) {
    polygons foreach (_.drawPolygon(xOffset, yOffset, gc, color))
  }

  def isInside(x: Int, y: Int) = polygons.exists(_.isInside(x, y))

}