package mr.merc.util

import org.scalatest.{FunSuite, Matchers}
import Divide._

class DivideTest extends FunSuite with Matchers {
  test("divide operation") {


    (10 divList 5) shouldBe List(2, 2, 2, 2, 2)
    (11 divList 5) shouldBe List(3, 2, 2, 2, 2)
    (3 divList 5) shouldBe List(1, 1, 1, 0, 0)
    (3 divList 1) shouldBe List(3)
  }

  test("divide with quota") {
    10 divList List(0.5, 0.5) shouldBe List(5, 5)
    11 divList List(0.5, 0.5) shouldBe List(6, 5)
    12 divList List(0.4, 0.4, 0.2) shouldBe List(5, 5, 2)
    13 divList List(0.4, 0.4, 0.2) shouldBe List(6, 5, 2)
  }
}
