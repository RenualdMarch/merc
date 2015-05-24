package mr.merc.image

import org.scalatest.FunSuite
import scalafx.scene.image.Image
import scalafx.scene.image.WritableImage
import scalafx.scene.paint.Color

class ImageUtilTest extends FunSuite {
	def createImage3x3:Image = {
	  val image = new WritableImage(3, 3)
	  val writer = image.pixelWrit
	  writer.setColor(0, 0, Color.Green)
	  writer.setColor(1, 0, Color.Aqua)
	  writer.setColor(2, 0, Color.Azure)
	  writer.setColor(0, 1, Color.Black)
	  writer.setColor(1, 1, Color.Blue)
	  writer.setColor(2, 1, Color.Coral)
	  writer.setColor(0, 2, Color.Wheat)
	  writer.setColor(1, 2, Color.White)
	  writer.setColor(2, 2, Color.WhiteSmoke)
	  image
	}
	
	def createImage2x2:Image = {
	  val image = new WritableImage(2, 2)
	  val writer = image.pixelWrit
	  writer.setColor(0, 0, Color.Yellow)
	  writer.setColor(1, 0, Color.YellowGreen)
	  writer.setColor(0, 1, Color.Olive)
	  writer.setColor(1, 1, Color.Orange)
	  image
	}
	
	test("mirror horizontally for 2x2 image") {
	  val image = ImageUtil.mirrorHorizontally(createImage2x2)
	  val reader = image.pixelReader.get
	  assert(reader.getColor(0, 0) === Color.Olive)
	  assert(reader.getColor(0, 1) === Color.Yellow)
	  assert(reader.getColor(1, 0) === Color.Orange)
	  assert(reader.getColor(1, 1) === Color.YellowGreen)
	}
	
	test("mirror horizontally for 3x3 image") {
	  val image = ImageUtil.mirrorHorizontally(createImage3x3)
	  val reader = image.pixelReader.get
	  assert(reader.getColor(0, 0) === Color.Wheat)
	  assert(reader.getColor(0, 1) === Color.Black)
	  assert(reader.getColor(0, 2) === Color.Green)
	  assert(reader.getColor(1, 0) === Color.White)
	  assert(reader.getColor(1, 1) === Color.Blue)
	  assert(reader.getColor(1, 2) === Color.Aqua)
	  assert(reader.getColor(2, 0) === Color.WhiteSmoke)
	  assert(reader.getColor(2, 1) === Color.Coral)
	  assert(reader.getColor(2, 2) === Color.Azure)
	}
	
	test("mirror vertically for 2x2 image") {
	  val image = ImageUtil.mirrorVertically(createImage2x2)
	  val reader = image.pixelReader.get
	  assert(reader.getColor(0, 0) === Color.YellowGreen)
	  assert(reader.getColor(0, 1) === Color.Orange)
	  assert(reader.getColor(1, 0) === Color.Yellow)
	  assert(reader.getColor(1, 1) === Color.Olive)
	}
	
	test("mirror vertically for 3x3 image") {
	  val image = ImageUtil.mirrorVertically(createImage3x3)
	  val reader = image.pixelReader.get
	  assert(reader.getColor(0, 0) === Color.Azure)
	  assert(reader.getColor(0, 1) === Color.Coral)
	  assert(reader.getColor(0, 2) === Color.WhiteSmoke)
	  assert(reader.getColor(1, 0) === Color.Aqua)
	  assert(reader.getColor(1, 1) === Color.Blue)
	  assert(reader.getColor(1, 2) === Color.White)
	  assert(reader.getColor(2, 0) === Color.Green)
	  assert(reader.getColor(2, 1) === Color.Black)
	  assert(reader.getColor(2, 2) === Color.Wheat)

	}
}