package mr.merc.map.view

import org.scalatest.FunSuite
import scalafx.geometry.Rectangle2D

class RectIntersectionHelperTest extends FunSuite {
  test("add/remove rects") {
    val helper = new RectIntersectionHelper[Int]
    helper.addRect(0, new Rectangle2D(0, 0, 5, 10))
    assert(helper.intersections(0) === Set())
    helper.addRect(1, new Rectangle2D(0, 0, 10, 5))
    assert(helper.intersections(0) === Set(1))
    assert(helper.intersections(1) === Set(0))
    helper.removeRect(0)
    assert(helper.intersections(1) === Set())
  }

  test("add/update") {
    val helper = new RectIntersectionHelper[Int]
    helper.addRect(0, new Rectangle2D(0, 0, 5, 10))
    assert(helper.intersections(0) === Set())
    helper.addRect(1, new Rectangle2D(0, 0, 10, 5))
    assert(helper.intersections(0) === Set(1))
    assert(helper.intersections(1) === Set(0))
    helper.addRect(0, new Rectangle2D(0, 0, 50, 10))
    assert(helper.intersections(0) === Set(1))
    assert(helper.intersections(1) === Set(0))
    helper.addRect(1, new Rectangle2D(100, 200, 50, 10))
    assert(helper.intersections(0) === Set())
    assert(helper.intersections(1) === Set())
  }
}