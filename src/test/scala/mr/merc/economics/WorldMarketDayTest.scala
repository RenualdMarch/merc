package mr.merc.economics

import mr.merc.economics.Population._
import mr.merc.economics.Products._
import mr.merc.politics.State
import org.scalatest.FunSuite

class WorldMarketDayTest extends FunSuite {

  var region1, region2, region3: EconomicRegion = _

  test("flow in one country") {

    // region1 produces grain and has liquor factory, region2 produces coal, region3 produces glass

    val state1 = new State("", KnightHuman, new StateBudget(0), TaxPolicy(CorporateTaxPolicy(0.2),
      SalaryTaxPolicy(Map[PopulationClass, Double](Lower -> 0.1, Middle -> 0.1, Upper -> 0.1)),
      SalesTaxPolicy(0), TariffTax(0.1), TransitTax(0.1)))

    // sales tax bug

    /*val state2 = new State(new StateBudget(), TaxPolicy(CorporateTaxPolicy(0),
      SalaryTaxPolicy(Map[PopulationClass, Double](Lower -> 0, Middle -> 0, Upper -> 0)),
      SalesTaxPolicy(0), TariffTax(0)))*/

    val initialPrices = Map[Products.Product, Double](Grain -> 10, Coal -> 15, Glass -> 10, Liquor -> 100, MachineParts -> 20)

    region1 = new EconomicRegion {
      override def owner: State = state1

      override def economicNeighbours: Set[EconomicRegion] = Set(region2, region3)

      override val regionMarket: RegionMarket = new RegionMarket(initialPrices)

      override val regionPopulation: RegionPopulation = new RegionPopulation(List(
        new Population(KnightHuman, Farmers, 1000, 200, 0),
        new Population(KnightHuman, Aristocrats, 10, 200, 0),
        new Population(KnightHuman, Craftsmen, 1000, 200, 0),
        new Population(KnightHuman, Capitalists, 20, 200, 0),
        new Population(KnightHuman, Traders, 200, 200, 0)))

      enterprises = Vector(new Farm(Grain, this, 1000, 1.0), new IndustrialFactory(this, Liquor, 1, 2000, 1000, 1, 1))
    }

    region2 = new EconomicRegion {
      override def owner: State = state1

      override def economicNeighbours: Set[EconomicRegion] = Set(region1, region3)

      override val regionMarket: RegionMarket = new RegionMarket(initialPrices)

      override val regionPopulation: RegionPopulation = new RegionPopulation(List(
        new Population(KnightHuman, Labourers, 1000, 500, 0),
        new Population(KnightHuman, Aristocrats, 10, 200, 0),
        new Population(KnightHuman, Traders, 200, 300, 0)))


      enterprises = Vector(new Mine(Coal, this, 1000, 1.0))
    }

    region3 = new EconomicRegion {
      override def owner: State = state1

      override def economicNeighbours: Set[EconomicRegion] = Set(region1, region2)

      override val regionMarket: RegionMarket = new RegionMarket(initialPrices)
      override val regionPopulation: RegionPopulation = new RegionPopulation(List(
        new Population(KnightHuman, Craftsmen, 1000, 300, 0),
        new Population(KnightHuman, Capitalists, 20, 400, 0),
        new Population(KnightHuman, Traders, 200, 300, 0)
      ))

      enterprises = Vector(new IndustrialFactory(this, Glass, 1, 2000, 1000, 1, 1))
    }

    val ws = new WorldState(List(region1, region2, region3))

    assert(ws.totalMoney === 7000)
    val day = new WorldMarketDay(Set(region1, region2, region3))
    day.trade()

    assert(ws.totalMoney === 7000)

  }

}
