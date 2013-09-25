package mr.merc.image

import scalafx.scene.image.Image
import scalafx.scene.image.WritableImage

object ImageUtil {
	def mirror(image:Image):Image = {
	  val writableImage = new WritableImage(image.width.value.toInt, image.height.value.toInt)
	  val writer = writableImage.pixelWrit
	  val reader = image.pixelReader.get
	  for (x <- 0 until image.width.value.toInt;
		   y <- 0 until image.height.value.toInt) {
		  val width = image.width.value.toInt
		  val mirroredX = width - x - 1
		  val argb = reader.getArgb(x, y)
		  writer.setArgb(mirroredX, y, argb)
	  }
	  
	  writableImage
	}
}