package mr.merc.map.hex.view

import org.scalatest.FunSuite
import mr.merc.map.terrain.Grass
import org.scalatest.BeforeAndAfter
import mr.merc.map.terrain.Sand
import mr.merc.map.hex._

class TerrainHexViewAdditiveTest extends FunSuite with BeforeAndAfter {
  
  test("Elements loaded correctly") {
    val elements = TerrainHexViewAdditiveElement.elements
    assert(elements.size === 14)
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, N, N)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, NE, NE)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, NW, NW)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, S, S)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, SE, SE)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, SW, SW)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, S, SW)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Grass, N, SE)))
    
    assert(elements.contains(new TerrainHexViewAdditiveElement(Sand, N, N)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Sand, NE, NE)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Sand, NW, NW)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Sand, S, S)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Sand, SE, SE)))
    assert(elements.contains(new TerrainHexViewAdditiveElement(Sand, SW, SW)))
  }
}