package mr.merc.map.world

import org.scalatest.FunSuite
import mr.merc.map.hex.TerrainHex
import mr.merc.map.objects.House
import mr.merc.map.terrain.Grass
import mr.merc.map.hex.TerrainHexField

class WorldMapTest extends FunSuite {
  test("province division") {
    val cities = Set((0, 0), (0, 3))

    def mapFunction(x: Int, y: Int): TerrainHex = {
      if (cities.contains(x, y)) {
        new TerrainHex(x, y, Grass, Some(House))
      } else {
        new TerrainHex(x, y, Grass)
      }
    }

    val field = new TerrainHexField(5, 5, mapFunction)
    import field._
    val world = new WorldMap(field, "provinceTest")
    val provinces = world.provinces.toList.sortBy(_.settlement.population)
    println(provinces)
    assert(provinces.size === 2)
    val firstProvince = provinces(0)
    assert(firstProvince.settlement === Settlement("name", "culture", 199))
    assert(firstProvince.hexes.size === 7)
    assert(firstProvince.containsHex(hex(0, 0)) === true)
    assert(firstProvince.containsHex(hex(0, 1)) === true)
    assert(firstProvince.containsHex(hex(0, 2)) === false)
    assert(firstProvince.containsHex(hex(3, 0)) === true)

    val secondProvince = provinces(1)
    assert(secondProvince.settlement === Settlement("name2", "culture2", 200))
    assert(secondProvince.hexes.size === 16)
    assert(secondProvince.containsHex(hex(0, 2)) === true)
    assert(secondProvince.containsHex(hex(0, 3)) === true)
    assert(secondProvince.containsHex(hex(0, 1)) === false)
    assert(secondProvince.containsHex(hex(3, 3)) === true)
  }
}