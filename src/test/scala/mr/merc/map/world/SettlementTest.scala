package mr.merc.map.world

import org.scalatest.FunSuite

class SettlementTest extends FunSuite {
  test("parsing test") {
    val result = Settlement.loadSettlements("testSettlements")

    assert(result(1, 2) === Settlement("name", "culture", 199))
    assert(result(3, 1) === Settlement("name2", "culture2", 200))
    assert(result.size === 2)
  }
}