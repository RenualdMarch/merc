package mr.merc.economics

import mr.merc.economics.Population._
import mr.merc.economics.Products._
import mr.merc.economics.TaxPolicy._
import mr.merc.politics.{Party, PoliticalViews, State}
import org.scalatest.FunSuite

class WorldMarketDayTest extends FunSuite {

  var region1, region2, region3: EconomicRegion = _

  test("flow in one country") {

    // region1 produces grain and has liquor factory, region2 produces coal, region3 produces glass

    val state1 = new State("", KnightHuman, 0, new PoliticalSystem(Party.absolute)) {
      override val taxPolicy: TaxPolicy = TaxPolicy(Map(CorporateTax -> 0.2, LowSalaryTax -> 0.1, MiddleSalaryTax -> 0.1,
        UpperSalaryTax -> 0.1,TariffTax -> 0.1, TransitTax -> 0.1))
    }

    /*val state2 = new State(new StateBudget(), TaxPolicy(CorporateTaxPolicy(0),
      SalaryTaxPolicy(Map[PopulationClass, Double](Lower -> 0, Middle -> 0, Upper -> 0)),
      SalesTaxPolicy(0), TariffTax(0)))*/

    val initialPrices = Products.AllProducts.map(_ -> 1000d).toMap ++ Map[Products.Product, Double](Grain -> 10, Coal -> 15, Glass -> 10, Liquor -> 100, MachineParts -> 20)

    region1 = new EconomicRegion {
      override def owner: State = state1

      override def economicNeighbours: Set[EconomicRegion] = Set(region2, region3)

      override val regionMarket: RegionMarket = new RegionMarket(initialPrices)

      override val regionPopulation: RegionPopulation = new RegionPopulation(List(
        new Population(KnightHuman, Farmers, 1000, 200, 0, PoliticalViews.averagePoliticalViews),
        new Population(KnightHuman, Aristocrats, 10, 200, 0, PoliticalViews.averagePoliticalViews),
        new Population(KnightHuman, Craftsmen, 1000, 200, 0, PoliticalViews.averagePoliticalViews),
        new Population(KnightHuman, Capitalists, 20, 200, 0, PoliticalViews.averagePoliticalViews),
        new Population(KnightHuman, Traders, 200, 200, 0, PoliticalViews.averagePoliticalViews)))

      enterprises = Vector(new Farm(Grain, this, 1000, 1.0), new IndustrialFactory(this, Liquor, 1, 2000, 1000, 1, 1))
    }

    region2 = new EconomicRegion {
      override def owner: State = state1

      override def economicNeighbours: Set[EconomicRegion] = Set(region1, region3)

      override val regionMarket: RegionMarket = new RegionMarket(initialPrices)

      override val regionPopulation: RegionPopulation = new RegionPopulation(List(
        new Population(KnightHuman, Labourers, 1000, 500, 0, PoliticalViews.averagePoliticalViews),
        new Population(KnightHuman, Aristocrats, 10, 200, 0, PoliticalViews.averagePoliticalViews),
        new Population(KnightHuman, Traders, 200, 300, 0, PoliticalViews.averagePoliticalViews)))


      enterprises = Vector(new Mine(Coal, this, 1000, 1.0))
    }

    region3 = new EconomicRegion {
      override def owner: State = state1

      override def economicNeighbours: Set[EconomicRegion] = Set(region1, region2)

      override val regionMarket: RegionMarket = new RegionMarket(initialPrices)
      override val regionPopulation: RegionPopulation = new RegionPopulation(List(
        new Population(KnightHuman, Craftsmen, 1000, 300, 0, PoliticalViews.averagePoliticalViews),
        new Population(KnightHuman, Capitalists, 20, 400, 0, PoliticalViews.averagePoliticalViews),
        new Population(KnightHuman, Traders, 200, 300, 0, PoliticalViews.averagePoliticalViews)
      ))

      enterprises = Vector(new IndustrialFactory(this, Glass, 1, 2000, 1000, 1, 1))
    }
    val regions = List(region1, region2, region3)

    val ws = new WorldStateMock(regions)

    assert(ws.totalMoney === 7000)
    val day = new WorldMarketDay(regions.toSet, 1)
    day.trade()
    regions.foreach { r =>
      r.enterprises.foreach { e =>
        assert(e.dayRecords.size === 1)
      }
      r.regionPopulation.pops.foreach { p =>
        assert(p.salary.size === 1)
      }
    }

    assert(ws.totalMoney === 7000)

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
