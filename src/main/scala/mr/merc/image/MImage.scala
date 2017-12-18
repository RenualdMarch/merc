package mr.merc.image

import scalafx.scene.image.Image
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.image.ImageView
import scalafx.scene.paint.Color
import scala.util.Try
import mr.merc.log.Logging
import mr.merc.util.CacheFactoryMap

import scalafx.geometry.Rectangle2D

object MImage {
  def apply(path: String, xOffset: Int, yOffset: Int, alpha: Float): MImage =
    new LazyMImage(path, xOffset, yOffset, alpha)

  def apply(path: String, xOffset: Int, yOffset: Int): MImage = apply(path, xOffset, yOffset, 1f)

  def apply(path: String): MImage = apply(path, 0, 0)

  def apply(image: Image, xOffset: Int, yOffset: Int, alpha: Float): MImage = new EagerMImage(image, xOffset, yOffset, alpha)

  def apply(image: Image, xOffset: Int, yOffset: Int): MImage = apply(image, xOffset, yOffset, 1f)

  def apply(image: Image): MImage = apply(image, 0, 0)

  val emptyImage = apply(ImageUtil.emptyImage)
}

abstract class MImage private[image] (val xOffset: Int, val yOffset: Int, val alpha: Float) extends Logging {

  val defaultAlpha = 1f
  def imagePath: Option[String]

  def image: Image

  def drawImage(gc: GraphicsContext, x: Int, y: Int) {
    gc.save()
    gc.globalAlpha = alpha
    gc.drawImage(image, x + xOffset, y + yOffset)
    gc.restore()
  }

  def centeredRect(x: Double, y: Double, width: Double, height: Double): Rectangle2D = {
    val actualX = x + xOffset + (width - this.width) / 2
    val actualY = y + yOffset + (height - this.height) / 2
    new Rectangle2D(actualX.toInt, actualY.toInt, image.width.value, image.height.value)
  }

  def drawCenteredImage(gc: GraphicsContext, x: Int, y: Int, width: Int, height: Int) {
    val rect = centeredRect(x, y, width, height)
    gc.drawImage(image, rect.minX toInt, rect.minY toInt)
  }

  def changeSoldierColor(color: Color) = new LazyChangedColorMImage(color, this, xOffset, yOffset, alpha)

  def width = image.width.value.toInt

  def height = image.height.value.toInt

  def changeAlpha(newAlpha: Float): MImage

  lazy val mirrorVertically = new LazyMirroredVerticallyImage(this, -xOffset, yOffset, alpha)

  lazy val mirrorHorizontally = new LazyMirroredHorizontallyImage(this, xOffset, -yOffset, alpha)

  def loadLazyImage() {
    try {
      image.width
    } catch {
      case e: Exception => {
        // since this may happen during tests execution, it may be alright to fail
        debug("Failed eagerly load image", e)
      }
    }
  }

  private val scaledCache = new CacheFactoryMap[Double, MImage]({ factor =>
    new ScaledMImage(image, factor, (xOffset * factor) toInt, (yOffset * factor) toInt, alpha)
  })

  def scaledImage(factor: Double): MImage = scaledCache(factor)
}