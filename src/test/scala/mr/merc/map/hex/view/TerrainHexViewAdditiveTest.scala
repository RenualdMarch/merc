package mr.merc.map.hex.view

import org.scalatest.FunSuite
import mr.merc.map.terrain._
import org.scalatest.BeforeAndAfter
import mr.merc.map.hex._
import mr.merc.map.terrain.TerrainType._

class TerrainHexViewAdditiveTest extends FunSuite with BeforeAndAfter {

  test("Elements loaded correctly") {
    val elements = TerrainHexViewAdditiveElement.elements.values.flatten.toList
    assert(elements.size > 100)

    val invalid = Set[TerrainType](BasicMountain, BasicHill, ShallowWater, Castle)

    TerrainType.list.foreach { tt =>

      if (!invalid.contains(tt)) {
        assert(elements.contains(new TerrainHexViewAdditiveElement(tt, N, N)))
        assert(elements.contains(new TerrainHexViewAdditiveElement(tt, NE, NE)))
        assert(elements.contains(new TerrainHexViewAdditiveElement(tt, NW, NW)))
        assert(elements.contains(new TerrainHexViewAdditiveElement(tt, S, S)))
        assert(elements.contains(new TerrainHexViewAdditiveElement(tt, SE, SE)))
        assert(elements.contains(new TerrainHexViewAdditiveElement(tt, SW, SW)))
      }
    }
  }
}