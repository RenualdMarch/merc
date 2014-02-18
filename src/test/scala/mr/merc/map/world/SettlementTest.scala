package mr.merc.map.world

import org.scalatest.FunSuite
import scala.xml.XML

class SettlementTest extends FunSuite {
  test("parsing test") {
    val xml = XML.load(getClass.getResourceAsStream("/maps/testSettlements.xml"))
    val result = WorldMap.loadSettlements(xml)

    assert(result(1, 2) === Settlement("name", "culture", 199))
    assert(result(3, 1) === Settlement("name2", "culture2", 200))
    assert(result.size === 2)
  }
}