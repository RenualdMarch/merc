package mr.merc.economics

import mr.merc.economics.Population._
import mr.merc.economics.Products.Grain
import mr.merc.politics.{PoliticalViews, State}
import org.scalatest.{BeforeAndAfter, FunSuite}

class ResourceGatheringTest extends FunSuite with BeforeAndAfter{

  var aristocrats: Population = _
  var magicalAristocrats: Population = _
  var workers: Population = _
  var region1:EconomicRegion = _
  var region2:EconomicRegion = _
  var region3:EconomicRegion = _
  var country: State = _

  val zeroPolicy = SalaryTaxPolicy(Map(Upper -> 0, Middle -> 0, Lower -> 0))

  before {
    aristocrats = new Population(LatinHuman, Aristocrats, 1000, 0, 0, PoliticalViews.averagePoliticalViews)
    aristocrats.newDay(zeroPolicy)
    magicalAristocrats = new Population(LatinHuman, MagicalAristocrats, 600, 0, 0, PoliticalViews.averagePoliticalViews)
    magicalAristocrats.newDay(zeroPolicy)
    workers = new Population(LatinHuman, Farmers, 4000, 0, 0, PoliticalViews.averagePoliticalViews)
    workers.newDay(zeroPolicy)
    country = new State("", KnightHuman, new StateBudget(0), TaxPolicy.zeroTaxes)

    region1 = new EconomicRegion {
      override def economicNeighbours: Set[EconomicRegion] = Set(region2, region1)

      override val regionMarket: RegionMarket = null
      override val regionPopulation: RegionPopulation = new RegionPopulation(List(aristocrats, magicalAristocrats, workers))

      override def toString: String = "region1"

      override def owner: State = country
    }

    region2 = new EconomicRegion {
      override def economicNeighbours: Set[EconomicRegion] = Set(region1, region3)

      override val regionMarket: RegionMarket = null
      override val regionPopulation: RegionPopulation = null

      override def toString: String = "region2"

      override def owner: State = country
    }

    region3 = new EconomicRegion {
      override def economicNeighbours: Set[EconomicRegion] = Set(region1, region2)

      override val regionMarket: RegionMarket = null
      override val regionPopulation: RegionPopulation = null

      override def toString: String = "region3"

      override def owner: State = country
    }
  }

  test("resource gathering path") {
    val resourceGathering = new Farm(Grain, region1, 1000d, 40)

    val prices = Map[Products.Product, Double](Grain -> 10)

    resourceGathering.newDay(CorporateTaxPolicy(0.1), 1)

    assert(resourceGathering.componentDemandRequests(prices) === Map())

    assert(resourceGathering.unsoldProducts === 1000)

    val sell = resourceGathering.sellProduct(Map(region1 -> EconomicRegionDemand(1000, 100),
      region2 -> EconomicRegionDemand(1000, 50)))
    assert(sell.mapValues(_.count) === Map(region1 -> 1000))

    val profit = FulfilledSupplyRequestProfit(
      FulfilledSupplyRequest(1000, 0, sell.head._2), 100)
    resourceGathering.receiveSellingResultAndMoney(region1, profit)

    assert(resourceGathering.unsoldProducts === 0)

    assert(resourceGathering.workforceEfficiencyDemand(prices) === 4000)

    resourceGathering.receiveWorkforceRequest(Map(workers -> 10000))

    resourceGathering.payMoneyToPops()
    assert(magicalAristocrats.moneyReserves === 16875)
    assert(aristocrats.moneyReserves === 28125)
    assert(workers.moneyReserves === 45000)

    assert(resourceGathering.payTaxes() === 10*1000)

    resourceGathering.produce()

    assert(resourceGathering.unsoldProducts === 10000 * 40)

    resourceGathering.endOfDay()
  }
}
