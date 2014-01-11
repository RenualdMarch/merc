package mr.merc.ui.common

import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.WritableImage
import scalafx.scene.SnapshotParameters
import scalafx.scene.image.Image
import scalafx.scene.paint.Color

object ImageHelper {
  def drawImage(width: Int, height: Int, params: SnapshotParameters = snapshotColor(Color(0, 0, 0, 0)))(f: GraphicsContext => Unit): Image = {
    val canvas = new Canvas
    canvas.width = width
    canvas.height = height
    val gc = canvas.graphicsContext2D
    f(gc)
    val image = new WritableImage(width, height)
    canvas.snapshot(params, image)
  }

  def snapshotColor(color: Color): SnapshotParameters = {
    val sp = new SnapshotParameters
    sp.fill = color
    sp
  }
}