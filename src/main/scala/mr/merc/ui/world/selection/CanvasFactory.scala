package mr.merc.ui.world.selection

import scalafx.geometry.Rectangle2D
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.paint.Color
import scalafx.scene.canvas.Canvas
import mr.merc.image.MImage

object CanvasFactory {

  def apply(rect: Rectangle2D, image: MImage, initialColor: Color = Color.BLACK): Canvas = {
    val canvas = new Canvas
    canvas.width = rect.width
    canvas.height = rect.height
    val ctx = canvas.graphicsContext2D
    ctx.fill = initialColor
    ctx.fillRect(0, 0, rect.width, rect.height)
    image.drawCenteredImage(ctx, 0, 0, rect.width.toInt, rect.height.toInt)
    canvas
  }
}