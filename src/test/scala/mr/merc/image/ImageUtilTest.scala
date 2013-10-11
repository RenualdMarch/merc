package mr.merc.image

import org.scalatest.FunSuite
import scalafx.scene.image.Image
import scalafx.scene.image.WritableImage
import scalafx.scene.paint.Color

class ImageUtilTest extends FunSuite {
	def createImage3x3:Image = {
	  val image = new WritableImage(3, 3)
	  val writer = image.pixelWrit
	  writer.setColor(0, 0, Color.GREEN)
	  writer.setColor(1, 0, Color.AQUA)
	  writer.setColor(2, 0, Color.AZURE)
	  writer.setColor(0, 1, Color.BLACK)
	  writer.setColor(1, 1, Color.BLUE)
	  writer.setColor(2, 1, Color.CORAL)
	  writer.setColor(0, 2, Color.WHEAT)
	  writer.setColor(1, 2, Color.WHITE)
	  writer.setColor(2, 2, Color.WHITESMOKE)
	  image
	}
	
	def createImage2x2:Image = {
	  val image = new WritableImage(2, 2)
	  val writer = image.pixelWrit
	  writer.setColor(0, 0, Color.YELLOW)
	  writer.setColor(1, 0, Color.YELLOWGREEN)
	  writer.setColor(0, 1, Color.OLIVE)
	  writer.setColor(1, 1, Color.ORANGE)
	  image
	}
	
	test("mirror horizontally for 2x2 image") {
	  val image = ImageUtil.mirrorHorizontally(createImage2x2)
	  val reader = image.pixelReader.get
	  assert(reader.getColor(0, 0) === Color.OLIVE)
	  assert(reader.getColor(0, 1) === Color.YELLOW)
	  assert(reader.getColor(1, 0) === Color.ORANGE)
	  assert(reader.getColor(1, 1) === Color.YELLOWGREEN)
	}
	
	test("mirror horizontally for 3x3 image") {
	  val image = ImageUtil.mirrorHorizontally(createImage3x3)
	  val reader = image.pixelReader.get
	  assert(reader.getColor(0, 0) === Color.WHEAT)
	  assert(reader.getColor(0, 1) === Color.BLACK)
	  assert(reader.getColor(0, 2) === Color.GREEN)
	  assert(reader.getColor(1, 0) === Color.WHITE)
	  assert(reader.getColor(1, 1) === Color.BLUE)
	  assert(reader.getColor(1, 2) === Color.AQUA)
	  assert(reader.getColor(2, 0) === Color.WHITESMOKE)
	  assert(reader.getColor(2, 1) === Color.CORAL)
	  assert(reader.getColor(2, 2) === Color.AZURE)
	}
	
	test("mirror vertically for 2x2 image") {
	  val image = ImageUtil.mirrorVertically(createImage2x2)
	  val reader = image.pixelReader.get
	  assert(reader.getColor(0, 0) === Color.YELLOWGREEN)
	  assert(reader.getColor(0, 1) === Color.ORANGE)
	  assert(reader.getColor(1, 0) === Color.YELLOW)
	  assert(reader.getColor(1, 1) === Color.OLIVE)
	}
	
	test("mirror vertically for 3x3 image") {
	  val image = ImageUtil.mirrorVertically(createImage3x3)
	  val reader = image.pixelReader.get
	  assert(reader.getColor(0, 0) === Color.AZURE)
	  assert(reader.getColor(0, 1) === Color.CORAL)
	  assert(reader.getColor(0, 2) === Color.WHITESMOKE)
	  assert(reader.getColor(1, 0) === Color.AQUA)
	  assert(reader.getColor(1, 1) === Color.BLUE)
	  assert(reader.getColor(1, 2) === Color.WHITE)
	  assert(reader.getColor(2, 0) === Color.GREEN)
	  assert(reader.getColor(2, 1) === Color.BLACK)
	  assert(reader.getColor(2, 2) === Color.WHEAT)

	}
}