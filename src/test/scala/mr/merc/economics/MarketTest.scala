package mr.merc.economics

import mr.merc.economics.WorldConstants.Market
import org.scalatest.{FunSuite, Matchers}

class MarketTest extends FunSuite with Matchers {

  test("formula") {
    Market.newPrice(1, 0, 0) shouldBe 1d
    Market.newPrice(1, 1, 0) shouldBe (Market.EmptyDemandPriceDecrease +- 0.1)
    Market.newPrice(1,0, 1) shouldBe (Market.EmptySupplyPriceIncrease +- 0.1)
    Market.newPrice(1,1, 1) shouldBe 1d
    Market.newPrice(1,10e5, 1) shouldBe (Market.EmptyDemandPriceDecrease +- 0.001)
    Market.newPrice(1,1, 10e5) shouldBe (Market.EmptySupplyPriceIncrease +- 0.001)

    Market.newPrice(1, 100, 0) shouldBe > (Market.newPrice(1, 1000, 0))
    Market.newPrice(1, 0, 100) shouldBe < (Market.newPrice(1, 0, 1000))

  }
}
