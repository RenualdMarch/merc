package mr.merc.economics

import mr.merc.map.{Grid, PossibleGrid}
import Products.Product
import mr.merc.economics.Population.{Culture, PopulationType}
import mr.merc.politics.State

trait EconomicRegion {

  def owner:State

  def economicNeighbours:Set[EconomicRegion]

  val regionMarket:RegionMarket

  val regionPopulation: RegionPopulation

  var enterprises:Vector[Enterprise] = Vector()
}

class EconomicGrid(region:EconomicRegion) extends PossibleGrid[EconomicRegion] {

  // TODO add cases of war and economic blockades
  override def isBlocked(t: EconomicRegion) = false

  def neighbours(t: EconomicRegion): List[EconomicRegion] = t.economicNeighbours.toList

  // currently we think that movement from one province to another takes same time
  override def price(from: EconomicRegion, to: EconomicRegion): Double = {
    val stateTransit = if (region.owner == from.owner) 0 else new StateTransitPart(from.owner).extractionPart
    1 + new TradersTransitPart(from).extractionPart + stateTransit
  }

  override def cellWhereItIsForbiddenToStop(t: EconomicRegion): Boolean = false

  override def cellWhereMovementMustBeStopped(t: EconomicRegion): Boolean = false
}

class RegionPopulation(initialPops: List[Population]) {

  private var currentPops = initialPops

  def generatePopDemands(prices:Map[Product, Double]):List[PopulationDemandRequest] = {
    currentPops.flatMap { pop =>
      pop.calculateDemands(prices).map { case (p, c) =>
        PopulationDemandRequest(pop, p, c)
      }
    }
  }

  def receivePopSalary(popType: PopulationType, money: Double): Unit = {
    val current = pops.filter(_.populationType == popType)
    val total = current.map(p => p.populationCount * p.efficiency).sum
    total match {
      case 0 => current.head.receiveSalary(money)
      case s => current.foreach { p =>
        p.receiveSalary(money * p.populationCount * p.efficiency / s)
      }
    }
  }

  def orderPop(populationType: PopulationType, requiredEfficiency: Double, culture: Option[Culture]): Map[Population, Double] = {
    val populations = pops.filter(_.populationType == populationType).filter(p => culture.forall(_ == p.culture))
    val totalEfficiency = populations.map(_.totalPopEfficiency).sum
    if (totalEfficiency == 0) {
      Map()
    } else {
      val part = requiredEfficiency / totalEfficiency
      if (part >= 1) {
        populations.map(p => p -> p.populationCount.toDouble).toMap
      } else if (part == 0) {
        Map()
      } else {
        populations.map(p => p -> p.populationCount * part).toMap
      }
    }
  }

  def pop(p: PopulationType, c: Culture): Population = {
    pops.find(pop => pop.populationType == p && pop.culture == c) match {
      case Some(x) => x
      case None =>
        val pop = new Population(c, p, 0, 0, 0)
        currentPops = pop :: currentPops
        pop
    }
  }

  def getPopTotalEfficiency(populationType: PopulationType): Double =
    pops.filter(_.populationType == populationType).map(_.totalPopEfficiency).sum

  def pops:List[Population] = currentPops

  def cultureMembers:Map[Culture, Int] = pops.groupBy(_.culture).mapValues(_.map(_.populationCount).sum)
}

class RegionMarket(initialPrices:Map[Product, Double]) {
  private var marketDaysForProduct = initialPrices.map { case (product, price) =>
    product -> new MarketDay(product, price)
  }

  def markets: Map[Product, MarketDay] = marketDaysForProduct

  def acceptDemands(demands:List[DemandRequest]): Unit = {
    demands.foreach { dr =>
      marketDaysForProduct(dr.product).acceptRequests(dr)
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

  def fulfilledSupply: Map[Product, List[FulfilledSupplyRequest]] = {
    marketDaysForProduct.map { case (p, market) =>
      p -> market.fulfilledSupply.getOrElse(Nil)
    }
  }

  def doTrade(products:List[Product]): Unit = {
    products.collect(marketDaysForProduct).foreach(_.calculateSupplyAndDemand())
  }

  def newMarketDay(): Unit = {
    marketDaysForProduct = marketDaysForProduct.map { case (p, m) =>
      p -> new MarketDay(p, m.tomorrowPrice.getOrElse(sys.error(s"Market for $p was not closed!")))
    }
  }

  def currentPrices: Map[Product, Double] = marketDaysForProduct.map { case (product, market) =>
    product -> market.price
  }
}
