package mr.merc.map.hex

import org.scalatest.FunSuite
import mr.merc.map.terrain.Grass
import org.scalatest.BeforeAndAfter

class TerrainHexViewAdditiveTest extends FunSuite with BeforeAndAfter {

  
  test("Elements loaded correctly") {
    val elements = TerrainHexViewAdditiveElement.elements
    assert(elements.size === 8)
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, Directions.N, Directions.N)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, Directions.NE, Directions.NE)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, Directions.NW, Directions.NW)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, Directions.S, Directions.S)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, Directions.SE, Directions.SE)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, Directions.SW, Directions.SW)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, Directions.S, Directions.SW)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, Directions.N, Directions.SE)))
  }
}