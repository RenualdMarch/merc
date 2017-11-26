package mr.merc.image
import scalafx.scene.image.Image

class ScaledMImage(initialImage: Image, factor: Double, xOffset: Int, yOffset: Int, alpha: Float) extends MImage(xOffset, yOffset, alpha) {

  override val imagePath:Option[String] = None

  override lazy val image:Image = MImageCache.scale(initialImage, factor)

  override def changeAlpha(newAlpha: Float): MImage = new ScaledMImage(initialImage, factor, xOffset, yOffset, newAlpha)

  override def scaledImage(factor: Double): MImage = throw new IllegalStateException("You are trying to resize already resized image!")

}
