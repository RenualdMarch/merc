package mr.merc.economics

import mr.merc.economics.Population._
import Culture._
import mr.merc.economics.Products._
import mr.merc.economics.TaxPolicy._
import mr.merc.politics.{Party, PoliticalViews, Province, State}
import org.scalatest.{FunSuite, Matchers}

class WorldMarketDayTest extends FunSuite with Matchers {

  var region1, region2, region3: EconomicRegion = _

  var worldStateEnterpriseActions = new WorldStateEnterpriseActions {
    override def playerState: State = ???

    override def controlledRegions: List[EconomicRegion] = regions

    override def regions: List[EconomicRegion] = List(region1, region2, region3)
  }

  test("flow in one country") {

    // region1 produces grain and has liquor factory, region2 produces coal, region3 produces glass

    val state1 = new State("", FrenchHuman, 0, new PoliticalSystem(Party.absolute))
    state1.taxPolicy.set(Map(CorporateTax -> 0.2, LowSalaryTax -> 0.1, MiddleSalaryTax -> 0.1,
      UpperSalaryTax -> 0.1,TariffTax -> 0.1, TransitTax -> 0.1))
    state1.budget.refreshTaxPolicy()

    /*val state2 = new State(new StateBudget(), TaxPolicy(CorporateTaxPolicy(0),
      SalaryTaxPolicy(Map[PopulationClass, Double](Lower -> 0, Middle -> 0, Upper -> 0)),
      SalesTaxPolicy(0), TariffTax(0)))*/

    val initialPrices = Products.AllProducts.map(_ -> 1000d).toMap ++ Map[Products.Product, Double](Grain -> 10, Coal -> 15, Liquor -> 100)

    region1 = new EconomicRegion {
      override def owner: State = state1

      override def economicNeighbours: Set[EconomicRegion] = Set(region2, region3)

      override val regionMarket: RegionMarket = new RegionMarket(initialPrices)
      override val regionWarriors: RegionWarriors = new RegionWarriors(Nil, economicNeighbours)
      override val regionPopulation: RegionPopulation = new RegionPopulation(List(
        new Population(FrenchHuman, Farmers, 1000, 200, 0, PoliticalViews.averagePoliticalViews),
        new Population(FrenchHuman, Aristocrats, 10, 200, 0, PoliticalViews.averagePoliticalViews),
        new Population(FrenchHuman, Craftsmen, 1000, 200, 0, PoliticalViews.averagePoliticalViews),
        new Population(FrenchHuman, Capitalists, 20, 200, 0, PoliticalViews.averagePoliticalViews),
        new Population(FrenchHuman, Traders, 200, 200, 0, PoliticalViews.averagePoliticalViews)))

      enterprises = Vector(new Farm(Grain, this, 1000, 1.0), new IndustrialFactory(this, Liquor, 1, 2000, 1000, 1, 1))
    }

    region2 = new EconomicRegion {
      override def owner: State = state1

      override def economicNeighbours: Set[EconomicRegion] = Set(region1, region3)

      override val regionMarket: RegionMarket = new RegionMarket(initialPrices)
      override val regionWarriors: RegionWarriors = new RegionWarriors(Nil, economicNeighbours)
      override val regionPopulation: RegionPopulation = new RegionPopulation(List(
        new Population(FrenchHuman, Labourers, 1000, 500, 0, PoliticalViews.averagePoliticalViews),
        new Population(FrenchHuman, Aristocrats, 10, 200, 0, PoliticalViews.averagePoliticalViews),
        new Population(FrenchHuman, Traders, 200, 300, 0, PoliticalViews.averagePoliticalViews)))


      enterprises = Vector(new Mine(Coal, this, 1000, 1.0))
    }

    region3 = new EconomicRegion {
      override def owner: State = state1

      override def economicNeighbours: Set[EconomicRegion] = Set(region1, region2)
      override val regionWarriors: RegionWarriors = new RegionWarriors(Nil, economicNeighbours)
      override val regionMarket: RegionMarket = new RegionMarket(initialPrices)
      override val regionPopulation: RegionPopulation = new RegionPopulation(List(
        new Population(FrenchHuman, Craftsmen, 1000, 300, 0, PoliticalViews.averagePoliticalViews),
        new Population(FrenchHuman, Capitalists, 20, 400, 0, PoliticalViews.averagePoliticalViews),
        new Population(FrenchHuman, Traders, 200, 300, 0, PoliticalViews.averagePoliticalViews)
      ))

      enterprises = Vector(new IndustrialFactory(this, Liquor, 1, 2000, 1000, 1, 1))
    }
    val regions = List(region1, region2, region3)

    val ws = new WorldStateMock(regions)

    assert(ws.totalMoney === 7000)
    val day = new WorldMarketDay(worldStateEnterpriseActions, 1)
    day.trade()
    regions.foreach { r =>
      r.enterprises.foreach { e =>
        assert(e.dayRecords.size === 1)
      }
      r.regionPopulation.pops.foreach { p =>
        assert(p.salary.size === 1)
      }
    }

    ws.totalMoney shouldBe 7000d +- 0.001

  }

  private class WorldStateMock(val regions: List[EconomicRegion]) {

    def totalMoney:Double = totalBudgetMoney + totalPopMoney + totalEnterpriseMoney

    def totalBudgetMoney:Double = {
      regions.map(_.owner).distinct.map(_.budget.moneyReserve).sum
    }

    def totalPopMoney: Double = {
      regions.flatMap(_.regionPopulation.pops).map(_.moneyReserves).sum
    }

    def totalEnterpriseMoney: Double = {
      regions.flatMap(_.enterprises).map(_.currentMoneyBalance).sum
    }
  }

}
