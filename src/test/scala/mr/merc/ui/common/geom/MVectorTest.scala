package mr.merc.ui.common.geom

import org.scalatest.FunSuite

class MVectorTest extends FunSuite {
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
}