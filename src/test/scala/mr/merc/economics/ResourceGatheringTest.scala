package mr.merc.economics

import mr.merc.economics.Population._
import mr.merc.economics.Products.Grain
import mr.merc.economics.TaxPolicy.CorporateTax
import mr.merc.politics.{Party, PoliticalViews, State}
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}

class ResourceGatheringTest extends FunSuite with BeforeAndAfter with Matchers {

  var aristocrats: Population = _
  var magicalAristocrats: Population = _
  var workers: Population = _
  var region1:EconomicRegion = _
  var region2:EconomicRegion = _
  var region3:EconomicRegion = _
  var country: State = _

  val zeroPolicy = TaxPolicy.zeroTaxes

  before {
    aristocrats = new Population(LatinHuman, Aristocrats, 1000, 0, 0, PoliticalViews.averagePoliticalViews)
    aristocrats.newDay(zeroPolicy, 1)
    magicalAristocrats = new Population(LatinHuman, MagicalAristocrats, 600, 0, 0, PoliticalViews.averagePoliticalViews)
    magicalAristocrats.newDay(zeroPolicy, 1)
    workers = new Population(LatinHuman, Farmers, 4000, 0, 0, PoliticalViews.averagePoliticalViews)
    workers.newDay(zeroPolicy,1)
    country = new State("", KnightHuman, 0, new PoliticalSystem(Party.absolute)) {
      override val taxPolicy: TaxPolicy = TaxPolicy.zeroTaxes
    }

    region1 = new EconomicRegion {
      override def economicNeighbours: Set[EconomicRegion] = Set(region2, region1)

      override val regionMarket: RegionMarket = null
      override val regionPopulation: RegionPopulation = new RegionPopulation(List(aristocrats, magicalAristocrats, workers))
      override val regionWarriors: RegionWarriors = null
      override def toString: String = "region1"

      override def owner: State = country
    }

    region2 = new EconomicRegion {
      override def economicNeighbours: Set[EconomicRegion] = Set(region1, region3)

      override val regionMarket: RegionMarket = null
      override val regionPopulation: RegionPopulation = null
      override val regionWarriors: RegionWarriors = null
      override def toString: String = "region2"

      override def owner: State = country
    }

    region3 = new EconomicRegion {
      override def economicNeighbours: Set[EconomicRegion] = Set(region1, region2)

      override val regionMarket: RegionMarket = null
      override val regionPopulation: RegionPopulation = null
      override val regionWarriors: RegionWarriors = null
      override def toString: String = "region3"

      override def owner: State = country
    }
  }

  test("resource gathering path") {
    val resourceGathering = new Farm(Grain, region1, 1000d, 40)

    val prices = Map[Products.Product, Double](Grain -> 10)

    resourceGathering.newDay(new TaxPolicy(Map(CorporateTax -> 0.1)), 1,1)

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
    aristocrats.moneyReserves shouldBe 45000d +- 000.1
    assert(magicalAristocrats.moneyReserves === 0)
    workers.moneyReserves shouldBe 45000d +- 000.1

    val payTaxes = resourceGathering.payTaxes()
    payTaxes.tax shouldBe CorporateTax
    payTaxes.taxed shouldBe 10*1000d +- 0.0001
    payTaxes.gross shouldBe 100000d +- 0.0001

    resourceGathering.produce()

    assert(resourceGathering.unsoldProducts === 10000 * 40)

    resourceGathering.endOfDay()
  }
}
