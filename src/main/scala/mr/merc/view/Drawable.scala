package mr.merc.view

import scalafx.scene.canvas.GraphicsContext

trait Drawable {
	def drawItself(gc:GraphicsContext)
}