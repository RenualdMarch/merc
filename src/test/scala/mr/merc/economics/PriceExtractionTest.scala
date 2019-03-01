package mr.merc.economics

import mr.merc.economics.Population._
import mr.merc.economics.TaxPolicy.{SalesTax, TariffTax, TransitTax}
import mr.merc.politics.{Party, PoliticalViews, State}
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}

class PriceExtractionTest extends FunSuite with BeforeAndAfter with Matchers {

  val state = new State("", KnightHuman, 0, Party.absolute) {
    override val taxPolicy: TaxPolicy = TaxPolicy(Map(SalesTax -> 0.5, TransitTax -> 0.1, TariffTax -> 0.2))
  }
  val traders = new Population(KnightHuman, Traders, 1000, 0, 0, PoliticalViews.averagePoliticalViews)

  val region = new EconomicRegion {
    override def owner: State = state

    override def economicNeighbours: Set[EconomicRegion] = Set()

    override val regionMarket: RegionMarket = null

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
    stateTransit = new StateTransitPart(state)
    stateSales = new StateSalesPart(state)
    stateTariff = new StateTariffPart(state)
  }

  test("no extractions") {
    val price = Price(110, Nil)
    assert(price.afterTaxesProfit === 110)
  }

  test("one extraction") {
    val price = Price(110, List(stateTransit))
    price.afterTaxesProfit should equal(100d +- 0.001)
    assert(stateTransit.extractionMoney === 10)
  }

  test("many extractions") {
    val price = Price(180, List(stateTransit, stateSales, stateTariff))
    price.afterTaxesProfit should equal(100d +- 0.001)
    assert(stateTransit.extractionMoney === 10)
    assert(stateSales.extractionMoney === 50)
    assert(stateTariff.extractionMoney === 20)
  }
}
