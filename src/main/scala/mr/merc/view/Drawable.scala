package mr.merc.view

import scalafx.scene.canvas.GraphicsContext
import scalafx.geometry.Rectangle2D

trait Drawable {
  def drawItself(gc: GraphicsContext, xOffset: Int, yOffset: Int)
  var dirtyRect: Option[Rectangle2D]
  def viewRect:Rectangle2D
}