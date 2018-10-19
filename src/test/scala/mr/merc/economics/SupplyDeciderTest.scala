package mr.merc.economics

import mr.merc.economics.Population.WesternHuman
import mr.merc.politics.State
import org.scalatest.FunSuite

class SupplyDeciderTest extends FunSuite{

  val country: State = new State("", WesternHuman, new StateBudget(0), TaxPolicy.zeroTaxes)

  private val region1:EconomicRegion = new EconomicRegion {
    override def economicNeighbours: Set[EconomicRegion] = Set(region2, region1)

    override val regionMarket:RegionMarket = null
    override val regionPopulation:RegionPopulation = null

    override def toString: String = "region1"

    override def owner: State = country
  }

  private val region2:EconomicRegion = new EconomicRegion {
    override def economicNeighbours: Set[EconomicRegion] = Set(region1, region3)

    override val regionMarket:RegionMarket = null
    override val regionPopulation:RegionPopulation = null

    override def toString: String = "region2"

    override def owner: State = country
  }

  private val region3:EconomicRegion = new EconomicRegion {
    override def economicNeighbours: Set[EconomicRegion] = Set(region1, region2)

    override val regionMarket:RegionMarket = null
    override val regionPopulation:RegionPopulation = null

    override def toString: String = "region3"

    override def owner: State = country
  }

  private val demand = Map(region1 -> EconomicRegionDemand(count = 100, profit = 200),
    region2 -> EconomicRegionDemand(count = 200, profit = 300),
    region3 -> EconomicRegionDemand(count = 300, profit = 100))

  test("start with not enough supply to fulfill demand") {
    val supplyDecider = new SupplyDecider()
    val map1 = supplyDecider.decideSupply(100, demand)
    assert(map1 === Map(region2 -> 100))
    val map2 = supplyDecider.decideSupply(550, demand)
    assert(map2 === Map(region2 -> 200.0, region1 -> 100.0, region3 -> 250.0))
    val map3 = supplyDecider.decideSupply(300, demand)
    assert(map3 === Map(region2 -> 200, region1 -> 100))
  }

  test("start with more supply than total demand") {
    val supplyDecider = new SupplyDecider()
    val map1 = supplyDecider.decideSupply(900, demand)
    assert(map1 === Map(region3 -> 450, region2 -> 300, region1 -> 150))
  }

  test("empty demand") {
    val supplyDecider = new SupplyDecider()
    val map1 = supplyDecider.decideSupply(900, Map())
    assert(map1 === Map())
  }

  test("yesterday correction for unsold") {
    val supplyDecider = new SupplyDecider()
    supplyDecider.receiveSupplyResults(Map(region2 -> FulfilledSupplyRequest(100, EnterpriseSupplyRequest(null, Products.Coal, 150))))
    val map1 = supplyDecider.decideSupply(200, demand)
    assert(map1 === Map(region2 -> 100, region1 -> 100))
    val map2 = supplyDecider.decideSupply(1000, demand)
    assert(map2 === Map(region2 -> 200, region1 -> 200, region3 -> 600))
  }

  test("yesterday correction for sold") {
    val supplyDecider = new SupplyDecider()
    supplyDecider.receiveSupplyResults(Map(region2 -> FulfilledSupplyRequest(100, EnterpriseSupplyRequest(null, Products.Coal, 100))))
    val map1 = supplyDecider.decideSupply(200, demand)
    assert(map1 === Map(region2 -> 125, region1 -> 75))
    val map2 = supplyDecider.decideSupply(100, demand)
    assert(map2 === Map(region2 -> 100))
  }
}
