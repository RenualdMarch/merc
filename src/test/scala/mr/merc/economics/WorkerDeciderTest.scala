package mr.merc.economics

import mr.merc.economics.Population._
import mr.merc.economics.Products.{Coal, Product, Ritual}
import mr.merc.politics.State
import org.scalatest.{BeforeAndAfter, FunSuite}

class WorkerDeciderTest extends FunSuite with BeforeAndAfter {

  val latinClergy = new Population(LatinHuman, Clergy, 1000, 0, 0)
  val westFarmer = new Population(LatinHuman, Farmers, 2000, 0, 0)
  val latinFarmer = new Population(WesternHuman, Farmers, 2000, 0, 0)
  val westCraftsmen = new Population(WesternHuman, Craftsmen, 3000, 0, 0)
  val latinCraftsmen = new Population(LatinHuman, Craftsmen, 1000, 0, 0)

  val populations = List(latinClergy, westFarmer, latinFarmer, westCraftsmen, latinCraftsmen)

  val country: State = new State(new StateBudget(0), TaxPolicy.zeroTaxes)

  val region = new EconomicRegion {
    override def economicNeighbours: Set[EconomicRegion] = ???

    override val regionMarket: RegionMarket = null

    override val regionPopulation: RegionPopulation = new RegionPopulation(populations)

    override def owner: State = country
  }

  class MockEnterprise(val possibleWorkers: PopulationType, val expectedSalaryPerEfficiency: Double,
                       efficiencyDemand: Double, val isResourceGathering: Boolean, name: String) extends Enterprise {

    val product: Products.Product = Coal

    val region: EconomicRegion = WorkerDeciderTest.this.region

    def produce() {}

    def sellProduct(demand: Map[EconomicRegion, EconomicRegionDemand]): Map[EconomicRegion, SupplyRequest] = ???

    def receiveSellingResultAndMoney(region: EconomicRegion, profit:FulfilledSupplyRequestProfit) = ???

    def payMoneyToPops() = ???

    def payTaxes(): Double = ???

    def endOfDay() = ???

    def newDay(taxPolicy: CorporateTaxPolicy): Unit = ???

    def owners: List[Population] = ???

    def receiveWorkforceRequest(result: Map[Population, Double]) = ???

    def workforceEfficiencyDemand(prices: Map[Product, Double]): Double = efficiencyDemand

    def componentDemandRequests(prices: Map[Product, Double]): Map[Product, DemandRequest] = ???

    override def currentMoneyBalance: Double = 0

    override def toString: String = name

    def receiveFulfilledDemandRequestsAndPayChecks(requests:Map[Product, FulfilledDemandRequest]) = ???
  }

  test("general case when demands are bigger then workers") {
    val factory1 = new MockEnterprise(Craftsmen, 10, 2000, false, "factory1")
    val factory2 = new MockEnterprise(Craftsmen, 15, 6000, false, "factory2")
    val farm1 = new MockEnterprise(Farmers, 20, 100000, true, "farm1")
    val farm2 = new MockEnterprise(Farmers, 25, 100000, true, "farm2")
    val church = new MockEnterprise(Clergy, 10, 100000, true, "church") {
      override val product: Product = Ritual(LatinHuman)
    }

    val enterprises = List(factory1, factory2, farm1, farm2, church)

    val workerDecider = new WorkerDecider(region, 0.4)
    val demands = workerDecider.supplyDemands(enterprises.map(e => e -> e.workforceEfficiencyDemand(Map())).toMap)

    assert(demands === Map(
      factory1 -> Map(latinCraftsmen -> 200, westCraftsmen -> 600),
      factory2 -> Map(latinCraftsmen -> 800, westCraftsmen -> 2400),
      farm2 -> Map(latinFarmer -> 1600, westFarmer -> 1600),
      church -> Map(latinClergy -> 1000.0),
      farm1 -> Map(latinFarmer -> 400, westFarmer -> 400)))
  }

  test("general case when demands are bigger then workers but cool factory have less places") {
    val factory1 = new MockEnterprise(Craftsmen, 10, 6000, false, "factory1")
    val factory2 = new MockEnterprise(Craftsmen, 15, 2000, false, "factory2")
    val farm1 = new MockEnterprise(Farmers, 20, 100000, true, "farm1")
    val farm2 = new MockEnterprise(Farmers, 25, 100000, true, "farm2")
    val church = new MockEnterprise(Clergy, 10, 100000, true, "church") {
      override val product: Product = Ritual(LatinHuman)
    }

    val enterprises = List(factory1, factory2, farm1, farm2, church)

    val workerDecider = new WorkerDecider(region)
    val demands = workerDecider.supplyDemands(enterprises.map(e => e -> e.workforceEfficiencyDemand(Map())).toMap)

    assert(demands === Map(
      factory1 -> Map(latinCraftsmen -> 500, westCraftsmen -> 1500),
      factory2 -> Map(latinCraftsmen -> 500, westCraftsmen -> 1500),
      farm2 -> Map(latinFarmer -> 1500, westFarmer -> 1500),
      church -> Map(latinClergy -> 1000.0),
      farm1 -> Map(latinFarmer -> 500, westFarmer -> 500)))
  }

  test("case when factories are absent") {
    val farm1 = new MockEnterprise(Farmers, 20, 100000, true, "farm1")
    val farm2 = new MockEnterprise(Farmers, 25, 100000, true, "farm2")
    val church = new MockEnterprise(Clergy, 10, 100000, true, "church") {
      override val product: Product = Ritual(LatinHuman)
    }

    val enterprises = List(farm1, farm2, church)

    val workerDecider = new WorkerDecider(region)
    val demands = workerDecider.supplyDemands(enterprises.map(e => e -> e.workforceEfficiencyDemand(Map())).toMap)

    assert(demands === Map(
      farm2 -> Map(latinFarmer -> 1500, westFarmer -> 1500),
      church -> Map(latinClergy -> 1000.0),
      farm1 -> Map(latinFarmer -> 500, westFarmer -> 500)))

  }

  test("case when demands much less than populations") {
    val factory1 = new MockEnterprise(Craftsmen, 10, 1000, false, "factory1")
    val factory2 = new MockEnterprise(Craftsmen, 15, 2000, false, "factory2")
    val farm1 = new MockEnterprise(Farmers, 20, 100000, true, "farm1")
    val farm2 = new MockEnterprise(Farmers, 25, 100000, true, "farm2")
    val church = new MockEnterprise(Clergy, 10, 100000, true, "church") {
      override val product: Product = Ritual(LatinHuman)
    }

    val enterprises = List(factory1, factory2, farm1, farm2, church)

    val workerDecider = new WorkerDecider(region)
    val demands = workerDecider.supplyDemands(enterprises.map(e => e -> e.workforceEfficiencyDemand(Map())).toMap)

    assert(demands === Map(
      factory1 -> Map(latinCraftsmen -> 250, westCraftsmen -> 750),
      factory2 -> Map(latinCraftsmen -> 500, westCraftsmen -> 1500),
      farm2 -> Map(latinFarmer -> 1500, westFarmer -> 1500),
      church -> Map(latinClergy -> 1000.0),
      farm1 -> Map(latinFarmer -> 500, westFarmer -> 500)))
  }

  test("clergy works only for its church") {
    val westClergy = new Population(WesternHuman, Clergy, 500, 0, 0)

    val region = new EconomicRegion {
      override def economicNeighbours: Set[EconomicRegion] = ???

      override val regionMarket: RegionMarket = null

      override val regionPopulation: RegionPopulation = new RegionPopulation(List(latinClergy, westClergy))

      override def owner: State = country
    }

    val westChurch = new MockEnterprise(Clergy, 10, 100000, true, "westChurch") {
      override val product: Product = Ritual(WesternHuman)
    }

    val latinChurch = new MockEnterprise(Clergy, 15, 100000, true, "latinChurch") {
      override val product: Product = Ritual(LatinHuman)
    }

    val workerDecider = new WorkerDecider(region)
    val map = workerDecider.supplyDemands(Map(westChurch -> 10000, latinChurch -> 10000))
    assert(map === Map(westChurch -> Map(westClergy -> 500), latinChurch -> Map(latinClergy -> 1000)))
  }
}
