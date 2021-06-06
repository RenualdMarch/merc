package mr.merc.economics

import mr.merc.economics.Culture.FrenchHuman
import mr.merc.politics.{Party, State}
import org.scalatest.FunSuite

class SupplyDeciderTest extends FunSuite{

  val country: State = new State("", FrenchHuman, 0, Party.absolute, 0)
  country.taxPolicy.set(TaxPolicy.zeroTaxes.taxPolicyValues)
  country.budget.refreshTaxPolicy()

  private val region1:EconomicRegion = new EconomicRegion {
    override def economicNeighbours: Set[EconomicRegion] = Set(region2, region1)
    override val regionWarriors: RegionWarriors = null
    override val regionMarket:RegionMarket = null
    override val regionPopulation:RegionPopulation = null

    override def toString: String = "region1"

    override def owner: State = country
  }

  private val region2:EconomicRegion = new EconomicRegion {
    override def economicNeighbours: Set[EconomicRegion] = Set(region1, region3)

    override val regionMarket:RegionMarket = null
    override val regionPopulation:RegionPopulation = null
    override val regionWarriors: RegionWarriors = null
    override def toString: String = "region2"

    override def owner: State = country
  }

  private val region3:EconomicRegion = new EconomicRegion {
    override def economicNeighbours: Set[EconomicRegion] = Set(region1, region2)

    override val regionMarket:RegionMarket = null
    override val regionPopulation:RegionPopulation = null
    override val regionWarriors: RegionWarriors = null
    override def toString: String = "region3"

    override def owner: State = country
  }

  private val demand = Map(region1 -> EconomicRegionDemand(count = 100, profit = 100),
    region2 -> EconomicRegionDemand(count = 200, profit = 200),
    region3 -> EconomicRegionDemand(count = 300, profit = 200))

  ignore("start with not enough supply to fulfill demand") {
    val supplyDecider = new SupplyDecider()
    val map1 = supplyDecider.decideSupply(100, demand)
    assert(map1 === Map(region1 -> 20, region2 -> 40, region3 -> 40))
  }

  ignore("start with more supply than total demand") {
    val supplyDecider = new SupplyDecider()
    val map1 = supplyDecider.decideSupply(1000, demand)
    assert(map1 === Map(region3 -> 400, region2 -> 400, region1 -> 200))
  }

  test("empty demand") {
    val supplyDecider = new SupplyDecider()
    val map1 = supplyDecider.decideSupply(900, Map())
    assert(map1 === Map())
  }

}
