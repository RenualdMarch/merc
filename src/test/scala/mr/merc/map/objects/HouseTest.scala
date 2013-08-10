package mr.merc.map.objects

import org.scalatest.FunSuite
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain.Grass

class HouseTest extends FunSuite {
  val field = new TerrainHexField(5, 5, (x:Int, y:Int) => {
    if (x == 1 && y == 1) {
      new TerrainHex(x, y, Grass, Some(House))
    } else {
      new TerrainHex(x, y, Grass)
    }
  })
  
  test("positive case"){
     val images = House.images(field.hex(1, 1), field)
     assert(images.size === 1)
     assert(images(0).imagePath.get.contains("house.png"))
  }
 
  test("negative case") {
     val images = House.images(field.hex(1, 2), field)
     assert(images.size === 0)
  }
}