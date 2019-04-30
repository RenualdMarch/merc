package mr.merc.economics

import mr.merc.economics.Population._
import mr.merc.economics.TaxPolicy.{SalesTax, TariffTax, TransitTax}
import mr.merc.politics.{Party, PoliticalViews, State}
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}

class PriceExtractionTest extends FunSuite with BeforeAndAfter with Matchers {

  val state = new State("", KnightHuman, 0, new PoliticalSystem(Party.absolute)) {
    override val taxPolicy: TaxPolicy = new TaxPolicy(Map(SalesTax -> 0.5, TransitTax -> 0.1, TariffTax -> 0.2))
  }
  val traders = new Population(KnightHuman, Traders, 1000, 0, 0, PoliticalViews.averagePoliticalViews)

  val region = new EconomicRegion {
    override def owner: State = state

    override def economicNeighbours: Set[EconomicRegion] = Set()

    override val regionMarket: RegionMarket = null
    override val regionWarriors: RegionWarriors = null
    override def bureaucratsPercentageFromMax: Double = 1d

    override val regionPopulation: RegionPopulation = new RegionPopulation(List(traders))
  }

  var tradersSales: Extraction = _
  var tradersTransit: Extraction = _
  var stateTransit: Extraction = _
  var stateSales: Extraction = _
  var stateTariff: Extraction = _

  before {
    tradersSales = new TradersSalesPart(region)
    tradersTransit = new TradersTransitPart(region)
    stateTransit = new StateTransitPart(state, region)
    stateSales = new StateSalesPart(state, region)
    stateTariff = new StateTariffPart(state, region)
  }

  test("no extractions") {
    val price = Price(110, Nil)
    assert(price.afterTaxesProfit === 110)
  }

  test("one extraction") {
    val price = Price(110, List(stateTransit))
    price.afterTaxesProfit shouldBe 100d +- 0.001
    stateTransit.extractionMoney shouldBe 10d +- 0.0001
  }

  test("many extractions") {
    val price = Price(180, List(stateTransit, stateSales, stateTariff))
    price.afterTaxesProfit shouldBe 100d +- 0.001
    stateTransit.extractionMoney shouldBe 10d +- 0.0001
    stateSales.extractionMoney shouldBe 50d +- 0.0001
    stateTariff.extractionMoney shouldBe 20d +- 0.0001
  }
}
