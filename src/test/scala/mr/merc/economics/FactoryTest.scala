package mr.merc.economics

import mr.merc.economics.Factory.FactoryRecord
import mr.merc.economics.Population._
import mr.merc.economics.Products._
import mr.merc.economics.TaxPolicy.CorporateTax
import mr.merc.politics.{Party, PoliticalViews, State}
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FunSuite}

class FactoryTest extends FunSuite with BeforeAndAfter with MockitoSugar {

  var capitalists: Population = _
  var region1:EconomicRegion = _
  var region2:EconomicRegion = _
  var region3:EconomicRegion = _
  var owner: State = _

  val zeroTaxes = TaxPolicy(Map())

  var testRegionMarket: RegionMarket = _

  before {
    capitalists = new Population(LatinHuman, Capitalists, 1000, 0, 0, PoliticalViews.averagePoliticalViews)
    capitalists.newDay(zeroTaxes)
    owner = new State("", KnightHuman, 0, new PoliticalSystem(Party.absolute)) {
      override val taxPolicy: TaxPolicy = TaxPolicy.zeroTaxes
    }
    testRegionMarket = mock[RegionMarket]

    region1 = new EconomicRegion {
      override def economicNeighbours: Set[EconomicRegion] = Set(region2, region1)

      override val regionMarket: RegionMarket = testRegionMarket
      override val regionPopulation: RegionPopulation = new RegionPopulation(List(capitalists))

      override def toString: String = "region1"

      override def owner: State = FactoryTest.this.owner
    }

    region2 = new EconomicRegion {
      override def economicNeighbours: Set[EconomicRegion] = Set(region1, region3)

      override val regionMarket: RegionMarket = testRegionMarket
      override val regionPopulation: RegionPopulation = null

      override def toString: String = "region2"

      override def owner: State = FactoryTest.this.owner
    }

    region3 = new EconomicRegion {
      override def economicNeighbours: Set[EconomicRegion] = Set(region1, region2)

      override val regionMarket: RegionMarket = testRegionMarket
      override val regionPopulation: RegionPopulation = null

      override def toString: String = "region3"

      override def owner: State = FactoryTest.this.owner
    }
  }

  test("factory usage general flow - happy path") {
    val factory = new IndustrialFactory(region1, Wine, 2, 1000000, 40000, 20, 1) {
      override val requiredMaintenance: Map[Product, Double] = Map(Coal -> 1000)
    }

    val prices = Map[Product, Double](Glass -> 10, Fruit -> 5, Coal -> 1)

    when(testRegionMarket.currentPrices).thenReturn(prices)

    factory.newDay(TaxPolicy(Map(CorporateTax -> 0.1)), 1)
    assert(factory.dayRecords === Vector())
    val requests = factory.componentDemandRequests(prices)
    assert(requests.mapValues(_.count) === Map(Glass -> 16000, Fruit -> 24000, Coal -> 1000))

    assert(factory.factoryStorage.unsoldProducts === 40000)
    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")

    val sell = factory.sellProduct(Map(region1 -> EconomicRegionDemand(40000, 100),
      region2 -> EconomicRegionDemand(40000, 50)))
    assert(sell.mapValues(_.count) === Map(region1 -> 40000))

    val fulfilledRequests = requests.map { case (p, r) =>
      FulfilledDemandRequest(r.count, prices(p), r)
    } toList

    assert(factory.currentMoneyBalance === 1000000)
    factory.buyDemandedProducts(fulfilledRequests)
    assert(factory.factoryStorage.unusedProducts === Map(Glass -> 16000, Fruit -> 24000, Coal -> 1000))

    val moneySpentOnResources = 16000 * 10 + 24000 * 5 + 1000
    val moneyEarned = 40000 * 100

    assert(factory.currentMoneyBalance === 1000000 - moneySpentOnResources)
    val moneyBalanceAfterDemand = factory.currentMoneyBalance
    val profit = FulfilledSupplyRequestProfit(FulfilledSupplyRequest(40000, 0, sell.head._2), 100)
    factory.receiveSellingResultAndMoney(region1, profit)
    assert(factory.factoryStorage.unsoldProducts === 0)
    assert(factory.currentMoneyBalance === moneyBalanceAfterDemand + moneyEarned)

    assert(factory.workforceEfficiencyDemand(prices) === 2000)

    val workers = new Population(LatinHuman, Craftsmen, 10000, 0, 0, PoliticalViews.averagePoliticalViews)
    workers.newDay(zeroTaxes)
    assert(workers.efficiency === 1)

    factory.receiveWorkforceRequest(Map(workers -> 2000))

    factory.payMoneyToPops()
    val taxes = (moneyEarned - moneySpentOnResources) * 0.1
    assert(factory.payTaxes() === TaxData(CorporateTax, 3719000, 371900))
    val maxReserves = 16000 * prices(Glass) + 24000 * prices(Fruit) + 1000 * prices(Coal)
    assert(factory.maxMoneyReserves === maxReserves)
    val moneyToSpend = moneyBalanceAfterDemand + moneyEarned - taxes - maxReserves

    assert(workers.moneyReserves === moneyToSpend * 0.5)
    assert(capitalists.moneyReserves === moneyToSpend * 0.5)
    assert(factory.currentMoneyBalance === maxReserves)

    factory.produce()

    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")
    assert(factory.factoryStorage.unsoldProducts === 40000)

    factory.endOfDay()

    assert(factory.dayRecords === Vector(FactoryRecord(
      produced = 40000,
      moneySpentOnResources = moneySpentOnResources,
      moneyOnWorkforceSalary = moneyToSpend * 0.5,
      moneyOnOwnersPayment = moneyToSpend * 0.5,
      corporateTax = taxes,
      moneyToFactoryBudget = maxReserves,
      peopleResources = Map(workers -> 2000),
      sold = Map(region1 -> profit),
      bought = fulfilledRequests,
      turn = 1
    )))
  }

  test("factory usage general flow - deficit path") {
    val factory = new IndustrialFactory(region1, Wine, 2, 2000, 40000, 20, 1) {
      override val requiredMaintenance: Map[Product, Double] = Map(Coal -> 1000)
    }
    val prices = Map[Product, Double](Glass -> 1, Fruit -> 1, Coal -> 1)
    when(testRegionMarket.currentPrices).thenReturn(prices)

    factory.newDay(TaxPolicy(Map(CorporateTax -> 0.1)), 1)
    assert(factory.dayRecords === Vector())
    val requests = factory.componentDemandRequests(prices)
    assert(requests.mapValues(_.count) === Map(Glass -> 400, Fruit -> 600, Coal -> 1000))

    assert(factory.factoryStorage.unsoldProducts === 40000)
    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")

    val sell = factory.sellProduct(Map(region1 -> EconomicRegionDemand(1000, 10),
      region2 -> EconomicRegionDemand(1000, 5)))
    assert(sell.mapValues(_.count) === Map(region1 -> 20000, region2 -> 20000))

    val fulfilledRequests = requests.map { case (p, r) =>
      FulfilledDemandRequest(r.count / 2 , prices(p), r)
    } toList

    assert(factory.currentMoneyBalance === 2000)
    factory.buyDemandedProducts(fulfilledRequests)
    assert(factory.factoryStorage.unusedProducts === Map(Glass -> 200, Fruit -> 300, Coal -> 500))

    val moneySpentOnResources = 1000
    val moneyEarned = 100
    assert(factory.factoryStorage.unsoldProducts === 40000)
    val profit1 = FulfilledSupplyRequestProfit(FulfilledSupplyRequest(50, 0, sell.head._2), 2)
    val profit2 = FulfilledSupplyRequestProfit(FulfilledSupplyRequest(0, 0, sell.head._2), 5)
    factory.receiveSellingResultAndMoney(region1, profit1)
    assert(factory.currentMoneyBalance === 2000 - moneySpentOnResources + moneyEarned)
    assert(factory.factoryStorage.unsoldProducts === 39950)
    factory.receiveSellingResultAndMoney(region2, profit2)
    assert(factory.factoryStorage.unsoldProducts === 39950)
    assert(factory.currentMoneyBalance === 2000 - moneySpentOnResources + moneyEarned)

    assert(factory.workforceEfficiencyDemand(prices) === 2.5)

    val workers = new Population(LatinHuman, Craftsmen, 10000, 0, 0, PoliticalViews.averagePoliticalViews)
    workers.newDay(zeroTaxes)
    assert(workers.efficiency === 1)

    factory.receiveWorkforceRequest(Map(workers -> 2.5))

    factory.payMoneyToPops()
    assert(factory.payTaxes() === TaxData(CorporateTax,0,0))
    assert(capitalists.moneyReserves === 0)
    assert(workers.moneyReserves === 0)

    assert(factory.factoryStorage.unsoldProducts === 39950)
    factory.produce()

    assert(factory.factoryStorage.unusedProducts === Map(Glass -> 183.0, Fruit -> 274.5, Coal -> 0.0))
    assert(factory.factoryStorage.unsoldProducts === 39992.5)

    factory.endOfDay()

    assert(factory.dayRecords === Vector(FactoryRecord(
      produced = 42.5,
      moneySpentOnResources = moneySpentOnResources,
      moneyOnWorkforceSalary = 0,
      moneyOnOwnersPayment = 0,
      corporateTax = 0,
      moneyToFactoryBudget = 0,
      peopleResources = Map(workers -> 2.5),
      sold = Map(region1 -> profit1, region2 -> profit2),
      bought = fulfilledRequests,
      turn = 1
    )))
  }

  test("no workers came, factory sold from storage") {
    val startingMoney = 1000000
    val factory = new IndustrialFactory(region1, Wine, 2, startingMoney, 40000, 20, 1) {
      override val requiredMaintenance: Map[Product, Double] = Map(Coal -> 1000)
    }

    val prices = Map[Product, Double](Glass -> 10, Fruit -> 5, Coal -> 1)
    when(testRegionMarket.currentPrices).thenReturn(prices)

    factory.newDay(TaxPolicy(Map(CorporateTax -> 0.1)), 1)
    assert(factory.dayRecords === Vector())
    val requests = factory.componentDemandRequests(prices)
    assert(requests.mapValues(_.count) === Map(Glass -> 16000, Fruit -> 24000, Coal -> 1000))

    assert(factory.factoryStorage.unsoldProducts === 40000)
    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")

    val sell = factory.sellProduct(Map(region1 -> EconomicRegionDemand(40000, 100),
      region2 -> EconomicRegionDemand(40000, 50)))
    assert(sell.mapValues(_.count) === Map(region1 -> 40000))

    val fulfilledRequests = requests.map { case (p, r) =>
      FulfilledDemandRequest(r.count, prices(p), r)
    }.toList

    assert(factory.currentMoneyBalance === 1000000)
    factory.buyDemandedProducts(fulfilledRequests)
    assert(factory.factoryStorage.unusedProducts === Map(Glass -> 16000, Fruit -> 24000, Coal -> 1000))

    val moneySpentOnResources = 16000 * 10 + 24000 * 5 + 1000
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
    assert(factory.payTaxes() === TaxData(CorporateTax,taxes * 10, taxes))

    factory.produce()

    assert(factory.factoryStorage.unusedProducts === Map(Glass -> 16000.0, Fruit -> 24000.0, Coal -> 0.0) )
    assert(factory.factoryStorage.unsoldProducts === 0)

    factory.endOfDay()

    assert(capitalists.moneyReserves === startingMoney + moneyEarned - moneySpentOnResources - taxes - factory.maxMoneyReserves)

    assert(factory.dayRecords === Vector(FactoryRecord(
      produced = 0,
      moneySpentOnResources = moneySpentOnResources,
      moneyOnWorkforceSalary = 0,
      moneyOnOwnersPayment = startingMoney + moneyEarned - moneySpentOnResources - taxes - factory.maxMoneyReserves,
      corporateTax = taxes,
      moneyToFactoryBudget = factory.maxMoneyReserves,
      peopleResources = Map(),
      sold = Map(region1 -> profit),
      bought = fulfilledRequests,
      turn = 1
    )))
  }

  test("factory usage general flow - happy path with output multiplier") {
    val factory = new IndustrialFactory(region1, Wine, 2, 1000000, 40000, 20, 10) {
      override val requiredMaintenance: Map[Product, Double] = Map(Coal -> 1000)
    }

    val prices = Map[Product, Double](Glass -> 10, Fruit -> 5, Coal -> 1)

    when(testRegionMarket.currentPrices).thenReturn(prices)

    factory.newDay(TaxPolicy(Map(CorporateTax -> 0.1)), 1)
    assert(factory.dayRecords === Vector())
    val requests = factory.componentDemandRequests(prices)
    assert(requests.mapValues(_.count) === Map(Glass -> 16000, Fruit -> 24000, Coal -> 1000))

    assert(factory.factoryStorage.unsoldProducts === 40000)
    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")

    val sell = factory.sellProduct(Map(region1 -> EconomicRegionDemand(40000, 100),
      region2 -> EconomicRegionDemand(400000, 50)))
    assert(sell.mapValues(_.count) === Map(region1 -> 40000))

    val fulfilledRequests = requests.map { case (p, r) =>
      FulfilledDemandRequest(r.count, prices(p), r)
    } toList

    assert(factory.currentMoneyBalance === 1000000)
    factory.buyDemandedProducts(fulfilledRequests)
    assert(factory.factoryStorage.unusedProducts === Map(Glass -> 16000, Fruit -> 24000, Coal -> 1000))

    val moneySpentOnResources = 16000 * 10 + 24000 * 5 + 1000
    val moneyEarned = 40000 * 100

    assert(factory.currentMoneyBalance === 1000000 - moneySpentOnResources)
    val moneyBalanceAfterDemand = factory.currentMoneyBalance
    val profit = FulfilledSupplyRequestProfit(FulfilledSupplyRequest(40000, 0, sell.head._2), 100)
    factory.receiveSellingResultAndMoney(region1, profit)
    assert(factory.factoryStorage.unsoldProducts === 0)
    assert(factory.currentMoneyBalance === moneyBalanceAfterDemand + moneyEarned)

    assert(factory.workforceEfficiencyDemand(prices) === 2000)

    val workers = new Population(LatinHuman, Craftsmen, 10000, 0, 0, PoliticalViews.averagePoliticalViews)
    workers.newDay(zeroTaxes)
    assert(workers.efficiency === 1)

    factory.receiveWorkforceRequest(Map(workers -> 2000))

    factory.payMoneyToPops()
    val taxes = (moneyEarned - moneySpentOnResources) * 0.1
    assert(factory.payTaxes() === TaxData(CorporateTax, taxes * 10, taxes))
    val maxReserves = 16000 * prices(Glass) + 24000 * prices(Fruit) + 1000 * prices(Coal)
    assert(factory.maxMoneyReserves === maxReserves)
    val moneyToSpend = moneyBalanceAfterDemand + moneyEarned - taxes - maxReserves

    assert(workers.moneyReserves === moneyToSpend * 0.5)
    assert(capitalists.moneyReserves === moneyToSpend * 0.5)
    assert(factory.currentMoneyBalance === maxReserves)

    factory.produce()

    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")
    assert(factory.factoryStorage.unsoldProducts === 400000)

    factory.endOfDay()

    assert(factory.dayRecords === Vector(FactoryRecord(
      produced = 400000,
      moneySpentOnResources = moneySpentOnResources,
      moneyOnWorkforceSalary = moneyToSpend * 0.5,
      moneyOnOwnersPayment = moneyToSpend * 0.5,
      corporateTax = taxes,
      moneyToFactoryBudget = maxReserves,
      peopleResources = Map(workers -> 2000),
      sold = Map(region1 -> profit),
      bought = fulfilledRequests,
      turn = 1
    )))
  }
}
