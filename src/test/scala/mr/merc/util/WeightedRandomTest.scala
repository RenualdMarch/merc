package mr.merc.util

import org.scalatest.FunSuite

class WeightedRandomTest extends FunSuite {
  test("case with one element") {
    val d = new WeightedRandom(Map(10 -> 100d))
    assert(d.nextRandomItem() === 10)
    assert(d.nextRandomItem() === 10)
    assert(d.nextRandomItem() === 10)
  }

  test("case with two similar elements") {
    val d = new WeightedRandom(Map(10 -> 1d, 20 -> 1d))
    var tens = 0
    var twenties = 0

    0 until 1000 foreach {_ =>
      if (d.nextRandomItem() == 10) {
        tens += 1
      } else {
        twenties += 1
      }
    }

    assert(tens < 550)
    assert(tens > 450)
    assert(twenties < 550)
    assert(twenties > 450)
  }

  test("case with two different elements") {
    val d = new WeightedRandom(Map(10 -> 0.9d, 20 -> 0.1d))
    var tens = 0
    var twenties = 0

    0 until 1000 foreach {_ =>
      if (d.nextRandomItem() == 10) {
        tens += 1
      } else {
        twenties += 1
      }
    }

    assert(tens < 950)
    assert(tens > 850)
    assert(twenties < 150)
    assert(twenties > 50)
  }
}
