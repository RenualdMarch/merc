package mr.merc.economics.util

import org.scalatest.FunSuite

class DistributionCalculatorTest extends FunSuite {

  test("when workers more than demand") {
    val dc = new DistributionCalculator[String](0.4, 0.6, x => x.toDouble)

    val result = dc.divide(1000, Map("1" -> 100, "2" -> 200, "3" -> 300))
    assert(result === Map("1" -> 100, "2" -> 200, "3" -> 300))
  }

  test("when workers less than demand") {
    val dc = new DistributionCalculator[String](0.6, 0.4, x => x.toDouble)

    val result = dc.divide(100, Map("1" -> 100, "2" -> 200, "3" -> 200))
    assert(result === Map("1" -> 20, "2" -> 20, "3" -> 60))
  }

  test("when order is less than equal distribution want to give") {
    val dc = new DistributionCalculator[String](0.6, 0.4, x => x.toDouble)

    val result = dc.divide(100, Map("1" -> 10, "2" -> 200, "3" -> 200))
    assert(result === Map("1" -> 10, "2" -> 25, "3" -> 65))
  }

  test("when first prioritized is less that equal distribution want to give") {
    val dc = new DistributionCalculator[String](0.6, 0.4, x => x.toDouble)

    val result = dc.divide(100, Map("3" -> 30, "1" -> 200, "2" -> 200))
    assert(result === Map("1" -> 20, "2" -> 50, "3" -> 30))
  }

  test("equal is 100%") {
    val dc = new DistributionCalculator[String](1.0, 0, x => x.toDouble)

    val result = dc.divide(300, Map("3" -> 50, "1" -> 200, "2" -> 200))
    assert(result === Map("1" -> 125, "2" -> 125, "3" -> 50))
  }

  test("priority is 100%") {
    val dc = new DistributionCalculator[String](0, 1.0, x => x.toDouble)

    val result = dc.divide(300, Map("3" -> 50, "1" -> 200, "2" -> 200))
    assert(result === Map("1" -> 50, "2" -> 200, "3" -> 50))
  }

  test("validation") {
    intercept[RuntimeException] {
      new DistributionCalculator[String](1.0, 0.5, x => x.toDouble)

    }
  }

}
