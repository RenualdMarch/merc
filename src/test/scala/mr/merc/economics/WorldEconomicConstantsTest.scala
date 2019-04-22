package mr.merc.economics

import org.scalatest.{FunSuite, Matchers}

class WorldEconomicConstantsTest extends FunSuite with Matchers {
  test("tax collection formula") {
    import WorldEconomicConstants.Taxes._
    taxCollectionPart(0) shouldBe ZeroBureaucracyTaxCollection +- 0.001
    taxCollectionPart(0.5) shouldBe HalfBureaucracyTaxCollection +- 0.001
    taxCollectionPart(1) shouldBe 1d +- 0.001
  }
}
