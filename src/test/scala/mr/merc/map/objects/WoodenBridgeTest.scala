package mr.merc.map.objects

import org.scalatest.FunSuite
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain.GreenGrass
import mr.merc.map.hex.TerrainHexField
import mr.merc.image.MImage

class MapObjectTest extends FunSuite {
  test("default bridge") {
	 val func = new InitFunc(Map(((1, 1) -> WoodenBridge)))
	 implicit val field = new TerrainHexField(5, 5, func)
	 val centralImage = image(1, 1)
	 val leftImage = image(0, 2)
	 val rightImage = image(2, 1)
	 val noImage1 = image(1, 0)
	 val noImage2 = image(0, 1)
	 val noImage3 = image(2, 2)
	 val noImage4 = image(1, 2)
	 
	 val noImage = List(noImage1, noImage2, noImage3, noImage4)
	 assert(noImage.flatten.isEmpty)
	 
	 assert(centralImage.size === 1)
	 assert(leftImage.size === 1)
	 assert(rightImage.size === 1)
	 assert(centralImage(0).imagePath.get.contains("/ne-sw.png"))
	 assert(leftImage(0).imagePath.get.contains("/end-ne.png"))
	 assert(rightImage(0).imagePath.get.contains("/end-sw.png")) 
  }
  
  test("long bridge") {
     val func = new InitFunc(Map(((1, 1) -> WoodenBridge), ((2, 2) -> WoodenBridge)))
     implicit val field = new TerrainHexField(5, 5, func)
     val centralImage1 = image(1, 1)
     val centralImage2 = image(2, 2)
     val leftImage = image(0, 1)
     val rightImage = image(3, 2)
     val noImage1 = image(1, 0)
     val noImage2 = image(2, 1)
     val noImage3 = image(3, 1)
     val noImage4 = image(0, 2)
     val noImage5 = image(1, 2)
     val noImage6 = image(2, 3)
     
     val noImage = List(noImage1, noImage2, noImage3, noImage4, noImage5, noImage6)
	 assert(noImage.flatten.isEmpty)
	 
	 assert(centralImage1.size === 1)
	 assert(centralImage2.size === 1)
	 assert(leftImage.size === 1)
	 assert(rightImage.size === 1)
	 assert(centralImage1(0).imagePath.get.contains("/se-nw.png"))
	 assert(centralImage2(0).imagePath.get.contains("/se-nw.png"))
	 assert(leftImage(0).imagePath.get.contains("/end-se.png"))
	 assert(rightImage(0).imagePath.get.contains("/end-nw.png")) 
  }
  
  class InitFunc(map:Map[(Int, Int), MapObject]) extends Function2[Int, Int, TerrainHex] {
    def apply(x:Int, y:Int) = new TerrainHex(x, y, GreenGrass, map.get(x, y))
  }
  
  private def image(x:Int, y:Int)(implicit field:TerrainHexField):List[MImage] = {
    WoodenBridge.images(field.hex(x, y), field)
  }
}