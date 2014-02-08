package mr.merc.map.hex.view

import org.scalatest.FunSuite
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import mr.merc.map.view.SoldiersDrawer

class TerrainHexFieldViewTest extends FunSuite {
  test("pixel coordinates to hex") {
    val field = new TerrainHexFieldView(new TerrainHexField(10, 10, TerrainHex.grassInit), new SoldiersDrawer())

    val firstHex = field.hexByPixelCoords(30, 30).get
    assert(firstHex.hex.x === 0)
    assert(firstHex.hex.y === 0)

    val secondHex = field.hexByPixelCoords(20, 90).get
    assert(secondHex.hex.x === 0)
    assert(secondHex.hex.y === 1)

    val thirdHex = field.hexByPixelCoords(0, 5)
    assert(thirdHex === None)

    val fourthHex = field.hexByPixelCoords(100, 0)
    assert(fourthHex === None)

    val expectedHex = field.hex(7, 4)
    val fifthHex = field.hexByPixelCoords(expectedHex.center._1, expectedHex.center._2)
    assert(fifthHex.get.hex.x === 7)
    assert(fifthHex.get.hex.y === 4)
  }

  test("black hexes creation - even case") {
    val field = new TerrainHexFieldView(new TerrainHexField(2, 2, TerrainHex.grassInit), new SoldiersDrawer())
    val blackHexes = field.blackHexes.map(h => (h.hex.x, h.hex.y))
    assert(blackHexes === Set((-1, 0), (-1, -1), (-1, 1), (1, -1), (1, 1), (2, 0), (2, 1)))
  }

  test("black hexes creation - odd case") {
    val field = new TerrainHexFieldView(new TerrainHexField(3, 2, TerrainHex.grassInit), new SoldiersDrawer())
    val blackHexes = field.blackHexes.map(h => (h.hex.x, h.hex.y))
    assert(blackHexes === Set((-1, 0), (-1, -1), (-1, 1), (1, -1), (1, 1), (3, 0), (3, 1), (3, -1)))
  }
}