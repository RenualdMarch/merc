package mr.merc.image

import mr.merc.conf.Conf
import org.imgscalr.Scalr
import org.imgscalr.Scalr.{Method, Mode}

import scalafx.embed.swing.SwingFXUtils
import scalafx.scene.image.Image
import scalafx.scene.image.WritableImage

object ImageUtil {
	def mirrorVertically(image:Image):Image = {
	  val writableImage = new WritableImage(image.width.value.toInt, image.height.value.toInt)
	  val writer = writableImage.pixelWriter
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
	
	def mirrorHorizontally(image:Image):Image = {
	  val writableImage = new WritableImage(image.width.value.toInt, image.height.value.toInt)
	  val writer = writableImage.pixelWriter
	  val reader = image.pixelReader.get
	  for (x <- 0 until image.width.value.toInt;
		   y <- 0 until image.height.value.toInt) {
		  val height = image.height.value.toInt
		  val mirroredY = height - y - 1
		  val argb = reader.getArgb(x, y)
		  writer.setArgb(x, mirroredY, argb)
	  }
	  
	  writableImage
	}
	
	def emptyImage:Image = {
	  val writableImage = new WritableImage(1, 1)
	  val writer = writableImage.pixelWriter
	  writer.setArgb(0, 0, 0)
	  return writableImage
	}

	def scale(image: Image, factor: Double): Image = {
		val bi = SwingFXUtils.fromFXImage(image, null)
		val result = Scalr.resize(bi, Method.ULTRA_QUALITY, Mode.AUTOMATIC, bi.getWidth * factor toInt, bi.getHeight * factor toInt)
		SwingFXUtils.toFXImage(result, null)
	}
}