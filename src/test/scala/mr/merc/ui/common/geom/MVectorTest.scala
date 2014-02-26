package mr.merc.ui.common.geom

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class MVectorTest extends FunSuite with ShouldMatchers {
  test("scalar product") {
    val result = MVector(1, 2) * MVector(3, 2)
    assert(result === 7)
  }

  test("number product") {
    val result = MVector(1, 2) * 5
    assert(result === MVector(5, 10))
  }

  test("plus") {
    val result = MVector(1, 2) + MVector(3, 5)
    assert(result === MVector(4, 7))
  }

  test("minus") {
    val result = MVector(1, 2) - MVector(3, 5)
    assert(result === MVector(-2, -3))
  }

  test("norm") {
    val result = MVector(3, 4).norm
    assert(result === MVector(0.6, 0.8))
  }

  test("ortho") {
    val result1 = MVector(2, 0).ortho
    val result2 = MVector(0, 1.5).ortho
    val result3 = MVector(4, 3).ortho
    assert(result1 * MVector(2, 0) === 0)
    assert(result1 === MVector(0, 1))
    assert(result2 * MVector(0, 1.5) === 0)
    assert(result2 === MVector(1, 0))
    result3 * MVector(4, 3) should be(0d +- 0.01)
    result3.x should be(0.6 +- 0.01)
    result3.y should be(-0.8 +- 0.01)
  }
}