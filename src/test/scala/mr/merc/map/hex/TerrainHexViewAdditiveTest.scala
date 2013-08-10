package mr.merc.map.hex

import org.scalatest.FunSuite
import mr.merc.map.terrain.Grass
import org.scalatest.BeforeAndAfter
import mr.merc.map.terrain.Sand

class TerrainHexViewAdditiveTest extends FunSuite with BeforeAndAfter {
  
  test("Elements loaded correctly") {
    val elements = TerrainHexViewAdditiveElement.elements
    assert(elements.size === 14)
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, Directions.N, Directions.N)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, Directions.NE, Directions.NE)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, Directions.NW, Directions.NW)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, Directions.S, Directions.S)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, Directions.SE, Directions.SE)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, Directions.SW, Directions.SW)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, Directions.S, Directions.SW)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, Directions.N, Directions.SE)))
    
    assert(elements.contains(new TerrainHexViewAdditiveElement(Sand, Directions.N, Directions.N)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Sand, Directions.NE, Directions.NE)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Sand, Directions.NW, Directions.NW)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Sand, Directions.S, Directions.S)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Sand, Directions.SE, Directions.SE)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Sand, Directions.SW, Directions.SW)))
  }
}