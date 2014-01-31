package mr.merc.map.reader

import org.scalatest.FunSuite
import mr.merc.map.terrain._
import mr.merc.map.objects.WoodenBridge
import mr.merc.map.objects.House

class WesnothMapReaderTest extends FunSuite {
  test("map1") {
    val reader = new WesnothMapReader
    val result = reader.readMap(getClass().getResourceAsStream("/maps/terrainTypes1.map"))
    assert(result.width === 3)
    assert(result.height === 4)
    result.hexes.foreach(h => assert(h.soldier.isEmpty))
    assert(result.hex(0, 0).terrain === Road)
    assert(result.hex(0, 0).mapObj === None)
    assert(result.hex(0, 1).terrain === Swamp)
    assert(result.hex(0, 1).mapObj === None)
    assert(result.hex(0, 2).terrain === Sand)
    assert(result.hex(0, 2).mapObj === None)
    assert(result.hex(0, 3).terrain === Mountain)
    assert(result.hex(0, 3).mapObj === None)

    assert(result.hex(1, 0).terrain === Water)
    assert(result.hex(1, 0).mapObj === None)
    assert(result.hex(1, 1).terrain === Grass)
    assert(result.hex(1, 1).mapObj === Some(WoodenBridge))
    assert(result.hex(1, 2).terrain === Hill)
    assert(result.hex(1, 2).mapObj === None)

    assert(result.hex(2, 0).terrain === Grass)
    assert(result.hex(2, 0).mapObj === Some(House))
    assert(result.hex(2, 1).terrain === Forest)
    assert(result.hex(2, 1).mapObj === None)
    assert(result.hex(2, 2).terrain === Grass)
    assert(result.hex(2, 2).mapObj === None)
    assert(result.hex(2, 3).terrain === Grass)
    assert(result.hex(0, 3).mapObj === None)

  }
}