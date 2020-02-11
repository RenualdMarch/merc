package mr.merc.economics

import mr.merc.economics.WorldConstants.Market
import org.scalatest.{FunSuite, Matchers}

class MarketTest extends FunSuite with Matchers {

  test("formula") {
    Market.priceChange(0, 0) shouldBe 1d
    Market.priceChange(1, 0) shouldBe Market.EmptyDemandPriceDecrease
    Market.priceChange(0, 1) shouldBe Market.EmptySupplyPriceIncrease
    Market.priceChange(1, 1) shouldBe 1d
    Market.priceChange(10e5, 1) shouldBe (Market.EmptyDemandPriceDecrease +- 0.001)
    Market.priceChange(1, 10e5) shouldBe (Market.EmptySupplyPriceIncrease +- 0.001)

  }
}
