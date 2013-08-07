package mr.merc.image

import scalafx.scene.image.Image
import scalafx.scene.canvas.GraphicsContext

object MImage {
  def apply(path:String, xOffset:Int, yOffset:Int, alpha:Float):MImage =
    new LazyMImage(path, xOffset, yOffset, alpha)

  def apply(path:String, xOffset:Int, yOffset:Int):MImage = apply(path, xOffset, yOffset, 1f)

  def apply(path:String):MImage = apply(path, 0, 0)

  def apply(image:Image, xOffset:Int, yOffset:Int, alpha:Float):MImage = new EagerMImage(image, xOffset, yOffset, alpha)

  def apply(image:Image, xOffset:Int, yOffset:Int):MImage = apply(image, xOffset, yOffset, 1f)

  def apply(image:Image):MImage = apply(image, 0, 0)
}

abstract class MImage private[image] (val xOffset:Int, val yOffset:Int, val alpha:Float) {

  val defaultAlpha = 1f
  def imagePath:Option[String]

  protected def image:Image
  
  def drawImage(gc:GraphicsContext, x:Int, y:Int) {
    gc.drawImage(image, x, y)
  }
  
  def drawCenteredImage(gc:GraphicsContext, x:Int, y:Int, width:Int, height:Int) {
    val actualX = x + (width - this.width) / 2
    val actualY = y + (height - this.height) / 2
    gc.drawImage(image, actualX, actualY)
  }

  def width = image.width.value

  def height = image.height.value
}