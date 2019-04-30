package mr.merc.economics

import org.scalatest.{FunSuite, Matchers}

class WorldConstantsTest extends FunSuite with Matchers {
  test("tax collection formula") {
    import WorldConstants.Taxes._
    taxCollectionPart(0) shouldBe ZeroBureaucracyTaxCollection +- 0.001
    taxCollectionPart(1) shouldBe 1d +- 0.001
    taxCollectionPart(0.5) shouldBe HalfBureaucracyTaxCollection +- 0.001


    val i = BigDecimal(0).to(1, 0.01)
    val j = BigDecimal(0).to(1, 0.01)
    i zip j.tail foreach { case (x1, x2) =>
      taxCollectionPart(x1.toDouble) should be < taxCollectionPart(x2.toDouble)
    }

  }

}
