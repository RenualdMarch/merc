package mr.merc.ui.common.geom

import org.scalatest.FunSuite

class LineTest extends FunSuite {
  test("cut from the beginning") {
    val initial = Line(3, 4, 9, 12)
    assert(initial.cutFromTheBeginning(5) === Line(6, 8, 9, 12))
  }

  test("cut from the end") {
    val initial = Line(3, 4, 9, 12)
    assert(initial.cutFromTheEnd(5) === Line(3, 4, 6, 8))
  }
}