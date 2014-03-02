package mr.merc.ui.common.geom

import org.scalatest.FunSuite

class PolygonTest extends FunSuite {
  test("check inside triangle") {
    val polygon = new Polygon((0, 0), (5, 5), (5, 0))
    assert(polygon.isInside(3, 0) === true)
    assert(polygon.isInside(1, 0.5) === true)
    assert(polygon.isInside(10, 10) === false)
    assert(polygon.isInside(1, 2) === false)
  }

  test("check inside rectangle") {
    val polygon = new Polygon((0, 0), (5, 0), (5, 5), (0, 5))
    assert(polygon.isInside(3, 0) === true)
    assert(polygon.isInside(1, 0.5) === true)
    assert(polygon.isInside(10, 10) === false)
    assert(polygon.isInside(1, 2) === true)
  }
}