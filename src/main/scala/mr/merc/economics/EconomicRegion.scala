package mr.merc.economics

import mr.merc.map.Grid
import Products.Product

trait EconomicRegion {

  def economicNeighbours:Set[EconomicRegion]

  val regionMarket:RegionMarket

  val pops: RegionPopulation
}

class EconomicGrid(region:EconomicRegion) extends Grid[EconomicRegion] {

  // TODO add cases of war and economic blockades
  override def isBlocked(t: EconomicRegion) = false

  def neighbours(t: EconomicRegion): Set[EconomicRegion] = t.economicNeighbours

  // currently we think that movement from one province to another takes same time
  override def price(from: EconomicRegion, to: EconomicRegion) = 1
}

class RegionPopulation(initialPops: List[Population]) {

  def generatePopDemands(prices:Map[Product, Double]):Map[Product, List[DemandRequest]] = {
    ???
  }

  def receiveFulfilledDemands(fulfilledDemands: Map[Product, List[FulfilledDemandRequest]]): Unit = {
    ???
  }
}

class RegionMarket(initialPrices:Map[Product, Double]) {
  private var marketDaysForProduct = initialPrices.map { case (product, prise) =>
    product -> new MarketDay(prise)
  }

  def markets: Map[Product, MarketDay] = marketDaysForProduct

  def acceptDemands(demands:Map[Product, List[DemandRequest]]): Unit = {
    demands.foreach { case (p, list) =>
      marketDaysForProduct(p).acceptRequests(list)
    }
  }

  def acceptSupply(supply: Map[Product, List[SupplyRequest]]): Unit = {
    supply.foreach { case (p, list) =>
      marketDaysForProduct(p).acceptRequests(list)
    }
  }

  def fulfilledDemands:Map[Product, List[FulfilledDemandRequest]] = {
    marketDaysForProduct.map { case (p, market) =>
      p -> market.fulfilledDemands.getOrElse(Nil)
    }
  }

  def doTrade(products:List[Product]): Unit = {
    products.map(marketDaysForProduct).foreach(_.calculateSupplyAndDemand())
  }

  def newMarketDay(): Unit = {
    marketDaysForProduct = marketDaysForProduct.map { case (p, m) =>
      p -> new MarketDay(m.tomorrowPrice.getOrElse(sys.error(s"Market for $p was not closed!")))
    }
  }

  def currentPrices: Map[Product, Double] = marketDaysForProduct.map { case (product, market) =>
    product -> market.price
  }
}
