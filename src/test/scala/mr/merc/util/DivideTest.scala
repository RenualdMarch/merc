package mr.merc.util

import org.scalatest.FunSuite

class DivideTest extends FunSuite {
  test("divide operation") {
    import Divide._

    assert((10 divList 5) === List(2, 2, 2, 2, 2))
    assert((11 divList 5) === List(3, 2, 2, 2, 2))
    assert((3 divList 5) === List(1, 1, 1, 0, 0))
    assert((3 divList 1) === List(3))
  }
}
