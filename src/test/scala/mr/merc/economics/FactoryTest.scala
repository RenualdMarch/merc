package mr.merc.economics

import mr.merc.economics.Factory.FactoryRecord
import mr.merc.economics.Population._
import mr.merc.economics.Culture._
import mr.merc.economics.Products._
import mr.merc.economics.TaxPolicy.CorporateTax
import mr.merc.politics.{Party, PoliticalViews, State}
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FunSuite, Inside, Matchers}

class FactoryTest extends FunSuite with BeforeAndAfter with MockitoSugar with Matchers with Inside {

  case object FruitTest extends FarmProduct
  case object CoalTest extends ResourceProduct
  case object GlassTest extends IndustryProduct(CoalTest -> 1)
  case object WineTest extends IndustryProduct(GlassTest -> 0.4, FruitTest -> 0.6)

  var capitalists: Population = _
  var craftsmen: Population = _
  var region1:EconomicRegion = _
  var region2:EconomicRegion = _
  var region3:EconomicRegion = _
  var owner: State = _

  val zeroTaxes = new TaxPolicy(Map())

  var testRegionMarket: RegionMarket = _

  before {
    capitalists = new Population(LatinHuman, Capitalists, 1000, 0, 0, PoliticalViews.averagePoliticalViews)
    capitalists.newDay(zeroTaxes, 1)
    craftsmen = new Population(LatinHuman, Craftsmen, 1000, 0, 0, PoliticalViews.averagePoliticalViews)
    craftsmen.newDay(zeroTaxes, 1)
    owner = new State("", FrenchHuman, 0, new PoliticalSystem(Party.absolute))
    owner.taxPolicy.set(TaxPolicy.zeroTaxes.taxPolicyValues)
    owner.budget.refreshTaxPolicy()

    testRegionMarket = mock[RegionMarket]

    region1 = new EconomicRegion {
      override def economicNeighbours: Set[EconomicRegion] = Set(region2, region1)

      override val regionMarket: RegionMarket = testRegionMarket
      override val regionPopulation: RegionPopulation = new RegionPopulation(List(capitalists, craftsmen))
      override val regionWarriors: RegionWarriors = null
      override def toString: String = "region1"

      override def owner: State = FactoryTest.this.owner
    }

    region2 = new EconomicRegion {
      override def economicNeighbours: Set[EconomicRegion] = Set(region1, region3)

      override val regionMarket: RegionMarket = testRegionMarket
      override val regionPopulation: RegionPopulation = null
      override val regionWarriors: RegionWarriors = null
      override def toString: String = "region2"

      override def owner: State = FactoryTest.this.owner
    }

    region3 = new EconomicRegion {
      override def economicNeighbours: Set[EconomicRegion] = Set(region1, region2)

      override val regionMarket: RegionMarket = testRegionMarket
      override val regionPopulation: RegionPopulation = null
      override val regionWarriors: RegionWarriors = null
      override def toString: String = "region3"

      override def owner: State = FactoryTest.this.owner
    }
  }

  test("factory usage general flow - happy path") {
    val factory = new IndustrialFactory(region1, WineTest, 2, 1000000, 40000, 20, 1)

    val prices = Map[Product, Double](GlassTest -> 10, FruitTest -> 5)

    when(testRegionMarket.currentPrices).thenReturn(prices)

    factory.newDay(new TaxPolicy(Map(CorporateTax -> 0.1)), 1, 1)
    assert(factory.dayRecords === Vector())
    val requests = factory.componentDemandRequests(prices)
    assert(requests.mapValues(_.count) === Map(GlassTest -> 16000, FruitTest -> 24000))

    assert(factory.factoryStorage.unsoldProducts === 40000)
    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")

    val sell = factory.sellProduct(Map(region1 -> EconomicRegionDemand(40000, 150),
      region2 -> EconomicRegionDemand(40000, 50)))
    assert(sell.mapValues(_.count) === Map(region1 -> 30000, region2 -> 10000))

    val fulfilledRequests = requests.map { case (p, r) =>
      FulfilledDemandRequest(r.count, prices(p), r)
    } toList

    assert(factory.currentMoneyBalance === 1000000)
    factory.buyDemandedProducts(fulfilledRequests)
    assert(factory.factoryStorage.unusedProducts === Map(GlassTest -> 16000, FruitTest -> 24000))

    val moneySpentOnResources = 16000 * 10 + 24000 * 5
    val moneyEarned = 40000 * 100

    assert(factory.currentMoneyBalance === 1000000 - moneySpentOnResources)
    val moneyBalanceAfterDemand = factory.currentMoneyBalance
    val profit = FulfilledSupplyRequestProfit(FulfilledSupplyRequest(40000, 0, sell.head._2), 100)
    factory.receiveSellingResultAndMoney(region1, profit)
    assert(factory.factoryStorage.unsoldProducts === 0)
    assert(factory.currentMoneyBalance === moneyBalanceAfterDemand + moneyEarned)

    assert(factory.workforceEfficiencyDemand(prices) === 2000)

    val workers = new Population(LatinHuman, Craftsmen, 10000, 0, 0, PoliticalViews.averagePoliticalViews)
    workers.newDay(zeroTaxes, 1)
    assert(workers.efficiency === 1)

    factory.receiveWorkforceRequest(Map(workers -> 2000))

    factory.payMoneyToPops()
    val taxes = (moneyEarned - moneySpentOnResources) * 0.1
    factory.payTaxes()
    owner.budget.dayReport.income(CorporateTax) shouldBe 372000d +- 0.0001
    owner.budget.dayReport.grossIncome(CorporateTax) shouldBe 3720000d +- 0.0001

    val maxReserves = 16000 * prices(GlassTest) + 24000 * prices(FruitTest)
    assert(factory.maxMoneyReserves === maxReserves)
    val moneyToSpend = moneyBalanceAfterDemand + moneyEarned - taxes - maxReserves

    assert(workers.moneyReserves === moneyToSpend * 0.5)
    assert(capitalists.moneyReserves === moneyToSpend * 0.5)
    factory.currentMoneyBalance shouldBe maxReserves +- 0.00001

    factory.produce()

    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")
    assert(factory.factoryStorage.unsoldProducts === 40000)

    factory.endOfDay()

    factory.dayRecords should have size 1
    val dr = factory.dayRecords.head

    inside(dr) { case FactoryRecord(produced, moneySpentOnResources,
      moneyOnWorkforceSalary, moneyOnOwnersPayment, corporateTax,
      moneyToFactoryBudget, peopleResources, bought, sold, turn) =>

      produced shouldBe 40000
      moneySpentOnResources shouldBe moneySpentOnResources
      moneyOnWorkforceSalary shouldBe moneyToSpend * 0.5
      moneyOnOwnersPayment shouldBe moneyToSpend * 0.5
      corporateTax shouldBe taxes +- 0.0001
      moneyToFactoryBudget shouldBe maxReserves
      peopleResources shouldBe Map(workers -> 2000)
      sold shouldBe Map(region1 -> profit)
      bought shouldBe fulfilledRequests
      turn shouldBe 1
    }
  }

  test("factory usage general flow - deficit path") {
    val factory = new IndustrialFactory(region1, WineTest, 2, 1000, 10, 20, 1)
    val prices = Map[Product, Double](GlassTest -> 1, FruitTest -> 1)
    when(testRegionMarket.currentPrices).thenReturn(prices)

    factory.newDay(new TaxPolicy(Map(CorporateTax -> 0.1)), 1, 1)
    assert(factory.dayRecords === Vector())
    val requests = factory.componentDemandRequests(prices)
    assert(requests.mapValues(_.count) === Map(GlassTest -> 400, FruitTest -> 600))

    assert(factory.factoryStorage.unsoldProducts === 10)
    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")

    val sell = factory.sellProduct(Map(region1 -> EconomicRegionDemand(1000, 10),
      region2 -> EconomicRegionDemand(1000, 10)))
    assert(sell.mapValues(_.count) === Map(region1 -> 5, region2 -> 5))

    val fulfilledRequests = requests.map { case (p, r) =>
      FulfilledDemandRequest(r.count / 2 , prices(p), r)
    } toList

    assert(factory.currentMoneyBalance === 1000)
    factory.buyDemandedProducts(fulfilledRequests)
    assert(factory.factoryStorage.unusedProducts === Map(GlassTest -> 200, FruitTest -> 300))

    val moneySpentOnResources = 500
    val moneyEarned = 100
    assert(factory.factoryStorage.unsoldProducts === 10)
    val profit1 = FulfilledSupplyRequestProfit(FulfilledSupplyRequest(5, 0, sell.head._2), 20)
    val profit2 = FulfilledSupplyRequestProfit(FulfilledSupplyRequest(0, 0, sell.head._2), 5)
    factory.receiveSellingResultAndMoney(region1, profit1)
    assert(factory.currentMoneyBalance === 1000 - moneySpentOnResources + moneyEarned)
    assert(factory.factoryStorage.unsoldProducts === 5)
    factory.receiveSellingResultAndMoney(region2, profit2)
    assert(factory.factoryStorage.unsoldProducts === 5)
    assert(factory.currentMoneyBalance === 1000 - moneySpentOnResources + moneyEarned)

    assert(factory.workforceEfficiencyDemand(prices) === 30)

    val workers = new Population(LatinHuman, Craftsmen, 10000, 0, 0, PoliticalViews.averagePoliticalViews)
    workers.newDay(zeroTaxes, 1)
    assert(workers.efficiency === 1)

    factory.receiveWorkforceRequest(Map(workers -> 2.5))

    factory.payMoneyToPops()
    factory.payTaxes()
    owner.budget.dayReport.income(CorporateTax) shouldBe 0
    owner.budget.dayReport.grossIncome(CorporateTax) shouldBe 0

    assert(capitalists.moneyReserves === 0)
    assert(workers.moneyReserves === 0)

    assert(factory.factoryStorage.unsoldProducts === 5)
    factory.produce()

    assert(factory.factoryStorage.unusedProducts === Map(GlassTest -> 180.0, FruitTest -> 270.0))
    assert(factory.factoryStorage.unsoldProducts === 55)

    factory.endOfDay()

    assert(factory.dayRecords === Vector(FactoryRecord(
      produced = 50,
      moneySpentOnResources = moneySpentOnResources,
      moneyOnWorkforceSalary = 0,
      moneyOnOwnersPayment = 0,
      corporateTax = 0,
      moneyToFactoryBudget = -400,
      peopleResources = Map(workers -> 2.5),
      sold = Map(region1 -> profit1, region2 -> profit2),
      bought = fulfilledRequests,
      turn = 1
    )))
  }

  test("no workers came, factory sold from storage") {
    val startingMoney = 1000000
    val factory = new IndustrialFactory(region1, WineTest, 2, startingMoney, 40000, 20, 1)

    val prices = Map[Product, Double](GlassTest -> 10, FruitTest -> 5)
    when(testRegionMarket.currentPrices).thenReturn(prices)

    factory.newDay(new TaxPolicy(Map(CorporateTax -> 0.1)), 1, 1)
    assert(factory.dayRecords === Vector())
    val requests = factory.componentDemandRequests(prices)
    assert(requests.mapValues(_.count) === Map(GlassTest -> 16000, FruitTest -> 24000))

    assert(factory.factoryStorage.unsoldProducts === 40000)
    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")

    val sell = factory.sellProduct(Map(region1 -> EconomicRegionDemand(40000, 100),
      region2 -> EconomicRegionDemand(40000, 0)))
    assert(sell.mapValues(_.count) === Map(region1 -> 40000))

    val fulfilledRequests = requests.map { case (p, r) =>
      FulfilledDemandRequest(r.count, prices(p), r)
    }.toList

    assert(factory.currentMoneyBalance === 1000000)
    factory.buyDemandedProducts(fulfilledRequests)
    assert(factory.factoryStorage.unusedProducts === Map(GlassTest -> 16000, FruitTest -> 24000))

    val moneySpentOnResources = 16000 * 10 + 24000 * 5
    val moneyEarned = 40000 * 100

    assert(factory.currentMoneyBalance === startingMoney - moneySpentOnResources)
    val moneyBalanceAfterDemand = factory.currentMoneyBalance
    val profit = FulfilledSupplyRequestProfit(FulfilledSupplyRequest(40000, 0, sell.head._2), 100)
    factory.receiveSellingResultAndMoney(region1, profit)
    assert(factory.factoryStorage.unsoldProducts === 0)
    assert(factory.currentMoneyBalance === moneyBalanceAfterDemand + moneyEarned)

    assert(factory.workforceEfficiencyDemand(prices) === 2000)

    factory.receiveWorkforceRequest(Map())

    factory.payMoneyToPops()
    val taxes = (moneyEarned - moneySpentOnResources) * 0.1
    factory.payTaxes()
    owner.budget.dayReport.income(CorporateTax) shouldBe taxes +- 0.00001
    owner.budget.dayReport.grossIncome(CorporateTax) shouldBe (taxes * 10) +- 0.00001


    factory.produce()

    assert(factory.factoryStorage.unusedProducts === Map(GlassTest -> 16000.0, FruitTest -> 24000.0) )
    assert(factory.factoryStorage.unsoldProducts === 0)

    factory.endOfDay()

    assert(capitalists.moneyReserves === startingMoney + moneyEarned - moneySpentOnResources - taxes - factory.maxMoneyReserves)

    factory.dayRecords should have size 1

    val dr = factory.dayRecords.head

    inside(dr) { case FactoryRecord(produced,  moneySpentOnResources, moneyOnWorkforceSalary, moneyOnOwnersPayment,
      corporateTax, moneyToFactoryBudget, peopleResources, bought, sold,  turn) =>
      produced shouldBe 0
      moneySpentOnResources shouldBe moneySpentOnResources
      moneyOnWorkforceSalary shouldBe 0
      moneyOnOwnersPayment shouldBe startingMoney + moneyEarned - moneySpentOnResources - taxes - factory.maxMoneyReserves
      corporateTax shouldBe taxes +- 0.00001
      moneyToFactoryBudget shouldBe factory.maxMoneyReserves
      peopleResources shouldBe Map()
      bought shouldBe fulfilledRequests
      sold shouldBe Map(region1 -> profit)
      turn shouldBe 1
    }
  }

  test("factory usage general flow - happy path with output multiplier") {
    val factory = new IndustrialFactory(region1, WineTest, 2, 1000000, 40000, 20, 10)

    val prices = Map[Product, Double](GlassTest -> 10, FruitTest -> 5)

    when(testRegionMarket.currentPrices).thenReturn(prices)

    factory.newDay(new TaxPolicy(Map(CorporateTax -> 0.1)), 1, 1)
    assert(factory.dayRecords === Vector())
    val requests = factory.componentDemandRequests(prices)
    assert(requests.mapValues(_.count) === Map(GlassTest -> 16000, FruitTest -> 24000))

    assert(factory.factoryStorage.unsoldProducts === 40000)
    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")

    val sell = factory.sellProduct(Map(region1 -> EconomicRegionDemand(40000, 100),
      region2 -> EconomicRegionDemand(400000, 0)))
    assert(sell.mapValues(_.count) === Map(region1 -> 40000))

    val fulfilledRequests = requests.map { case (p, r) =>
      FulfilledDemandRequest(r.count, prices(p), r)
    } toList

    assert(factory.currentMoneyBalance === 1000000)
    factory.buyDemandedProducts(fulfilledRequests)
    assert(factory.factoryStorage.unusedProducts === Map(GlassTest -> 16000, FruitTest -> 24000))

    val moneySpentOnResources = 16000 * 10 + 24000 * 5
    val moneyEarned = 40000 * 100

    assert(factory.currentMoneyBalance === 1000000 - moneySpentOnResources)
    val moneyBalanceAfterDemand = factory.currentMoneyBalance
    val profit = FulfilledSupplyRequestProfit(FulfilledSupplyRequest(40000, 0, sell.head._2), 100)
    factory.receiveSellingResultAndMoney(region1, profit)
    assert(factory.factoryStorage.unsoldProducts === 0)
    assert(factory.currentMoneyBalance === moneyBalanceAfterDemand + moneyEarned)

    assert(factory.workforceEfficiencyDemand(prices) === 2000)

    val workers = new Population(LatinHuman, Craftsmen, 10000, 0, 0, PoliticalViews.averagePoliticalViews)
    workers.newDay(zeroTaxes, 1)
    assert(workers.efficiency === 1)

    factory.receiveWorkforceRequest(Map(workers -> 2000))

    factory.payMoneyToPops()
    val taxes = (moneyEarned - moneySpentOnResources) * 0.1
    factory.payTaxes()
    owner.budget.dayReport.income(CorporateTax) shouldBe taxes +- 0.00001
    owner.budget.dayReport.grossIncome(CorporateTax) shouldBe (taxes * 10) +- 0.00001


    val maxReserves = 16000 * prices(GlassTest) + 24000 * prices(FruitTest)
    assert(factory.maxMoneyReserves === maxReserves)
    val moneyToSpend = moneyBalanceAfterDemand + moneyEarned - taxes - maxReserves

    assert(workers.moneyReserves === moneyToSpend * 0.5)
    assert(capitalists.moneyReserves === moneyToSpend * 0.5)
    factory.currentMoneyBalance shouldBe maxReserves +- 0.00001

    factory.produce()

    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")
    assert(factory.factoryStorage.unsoldProducts === 400000)

    factory.endOfDay()

    factory.dayRecords should have size 1

    val dr = factory.dayRecords.head

    inside(dr) { case FactoryRecord(produced,  moneySpentOnResources, moneyOnWorkforceSalary, moneyOnOwnersPayment,
    corporateTax, moneyToFactoryBudget, peopleResources, bought, sold,  turn) =>
      produced shouldBe 400000
      moneySpentOnResources shouldBe moneySpentOnResources
      moneyOnWorkforceSalary shouldBe moneyToSpend * 0.5
      moneyOnOwnersPayment shouldBe moneyToSpend * 0.5
      corporateTax shouldBe taxes +- 0.00001
      moneyToFactoryBudget shouldBe maxReserves
      peopleResources shouldBe Map(workers -> 2000)
      bought shouldBe fulfilledRequests
      sold shouldBe Map(region1 -> profit)
      turn shouldBe 1
    }
  }

  test("bankrupt factory") {
    val factory1 = new IndustrialFactory(region1, WineTest, 2, 1000000, 40000, 20, 10)

    val factory2 = new IndustrialFactory(region1, WineTest, 2, 0, 0, 20, 10)

    assert(factory1.isBankrupt === false)
    assert(factory2.isBankrupt === true)
  }
}
