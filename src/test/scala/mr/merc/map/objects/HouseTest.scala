package mr.merc.map.objects

import mr.merc.economics.Culture.LatinHuman
import org.scalatest.FunSuite
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain.GreenGrass

class HouseTest extends FunSuite {
  val field = new TerrainHexField(5, 5, (x:Int, y:Int) => {
    if (x == 1 && y == 1) {
      new TerrainHex(x, y, GreenGrass, Some(SummerHouse(LatinHuman)))
    } else {
      new TerrainHex(x, y, GreenGrass)
    }
  })
  
  test("positive case"){
     val images = SummerHouse(LatinHuman).images(field.hex(1, 1), field)
     assert(images.size === 1)
     assert(images.head.imagePath.get.contains(".png"))
  }

  test("negative case") {
     val images = SummerHouse(LatinHuman).images(field.hex(1, 2), field)
     assert(images.size === 0)
  }
}