package mr.merc.map.world

import org.scalatest.FunSuite
import mr.merc.map.hex.TerrainHex
import mr.merc.map.objects.House
import mr.merc.map.terrain.Grass
import mr.merc.map.hex.TerrainHexField
import scala.xml.XML
import mr.merc.world.Culture

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

    val xml = XML.load(getClass.getResourceAsStream("/maps/provinceTest.xml"))
    import field._
    val provinces = WorldMap.calculateProvinces(field, WorldMap.loadSettlements(xml)).toList.sortBy(_.settlement.population)
    assert(provinces.size === 2)
    val firstProvince = provinces(0)
    assert(firstProvince.settlement === Settlement("name", "culture", 199))
    assert(firstProvince.hexes.size === 11)
    assert(firstProvince.containsHex(hex(0, 0)) === true)
    assert(firstProvince.containsHex(hex(0, 1)) === true)
    assert(firstProvince.containsHex(hex(0, 2)) === false)
    assert(firstProvince.containsHex(hex(3, 0)) === true)

    val secondProvince = provinces(1)
    assert(secondProvince.settlement === Settlement("name2", "culture2", 200))
    assert(secondProvince.hexes.size === 12)
    assert(secondProvince.containsHex(hex(0, 2)) === true)
    assert(secondProvince.containsHex(hex(0, 3)) === true)
    assert(secondProvince.containsHex(hex(0, 1)) === false)
    assert(secondProvince.containsHex(hex(3, 3)) === true)
  }

  test("countries creation") {
    val cities = Set((0, 0), (0, 3), (3, 2))

    def mapFunction(x: Int, y: Int): TerrainHex = {
      if (cities.contains(x, y)) {
        new TerrainHex(x, y, Grass, Some(House))
      } else {
        new TerrainHex(x, y, Grass)
      }
    }

    val field = new TerrainHexField(5, 5, mapFunction)
    import field._
    val world = WorldMap.load(field, "testSettlements2")
    assert(world.countries.size === 2)
    val countries = world.countries.sortBy(_.nameKey)
    assert(countries(0).nameKey === "country1")
    assert(countries(0).culture === Culture("culture2"))
    assert(countries(0).provinces.map(_.settlement.nameKey) === Set("name2", "name3"))
    assert(countries(1).nameKey === "country2")
    assert(countries(1).culture === Culture("culture"))
    assert(countries(1).provinces.map(_.settlement.nameKey) === Set("name"))

  }

  // 1 should be subtracted from each coordinate
  test("from editor to map") {
    val world = WorldMap.load("citiesTest")
    assert(world.provinces.size === 2)
    assert(world.countries.size === 2)
  }

  test("province connections") {
    val cities = Set((0, 0), (0, 4), (4, 4), (4, 0), (2, 2))

    def mapFunction(x: Int, y: Int): TerrainHex = {
      if (cities.contains(x, y)) {
        new TerrainHex(x, y, Grass, Some(House))
      } else {
        new TerrainHex(x, y, Grass)
      }
    }

    val field = new TerrainHexField(6, 6, mapFunction)
    import field._
    val world = WorldMap.load(field, "provinceConnections")
    val province00 = world.provinceByHex(hex(0, 0))
    val province04 = world.provinceByHex(hex(0, 4))
    val province44 = world.provinceByHex(hex(4, 4))
    val province40 = world.provinceByHex(hex(4, 0))
    val province22 = world.provinceByHex(hex(2, 2))

    assert(world.provinceConnections(province00).toSet === Set((province04, 4), (province22, 3), (province40, 4)))
  }
}