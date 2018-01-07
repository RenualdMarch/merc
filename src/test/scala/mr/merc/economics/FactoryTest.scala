package mr.merc.economics

import mr.merc.economics.Factory.FactoryRecord
import mr.merc.economics.Population.{Capitalists, Craftsmen, LatinHuman, Slaves}
import mr.merc.economics.Products.{Fruit, Glass, MachineParts, Product, Wine}
import org.scalatest.FunSuite

class FactoryTest extends FunSuite {

  private val region1:EconomicRegion = new EconomicRegion {
    override def economicNeighbours: Set[EconomicRegion] = Set(region2, region1)
    override val regionMarket:RegionMarket = null
    override val pops:RegionPopulation = null
    override def toString: String = "region1"
  }

  private val region2:EconomicRegion = new EconomicRegion {
    override def economicNeighbours: Set[EconomicRegion] = Set(region1, region3)
    override val regionMarket:RegionMarket = null
    override val pops:RegionPopulation = null
    override def toString: String = "region2"
  }

  private val region3:EconomicRegion = new EconomicRegion {
    override def economicNeighbours: Set[EconomicRegion] = Set(region1, region2)
    override val regionMarket:RegionMarket = null
    override val pops:RegionPopulation = null
    override def toString: String = "region3"
  }

  test("factory usage general flow - happy path") {
    val factory = new IndustrialFactory(Wine, 2, 1000000)

    val prices = Map[Product, Double](Glass -> 10, Fruit -> 5, MachineParts -> 1)

    factory.newDay(FactoryTaxPolicy(0.1))
    assert(factory.dayRecords === Vector())
    val requests = factory.componentDemandRequests(prices)
    assert(requests.mapValues(_.count) === Map(Glass -> 16000, Fruit -> 24000, MachineParts -> 1000))

    assert(factory.factoryStorage.unsoldProducts === 40000)
    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")

    val sell = factory.sellProduct(Map(region1 -> EconomicRegionDemand(40000, 100),
      region2 -> EconomicRegionDemand(40000, 50)))
    assert(sell.mapValues(_.count) === Map(region1 -> 40000))

    val fulfilledRequests = requests.transform { case (p, r) =>
      FulfilledDemandRequest(r.count, 0, prices(p), r)
    }

    assert(factory.currentMoneyBalance === 1000000)
    factory.receiveFulfilledDemandRequestsAndPayChecks(fulfilledRequests)
    assert(factory.factoryStorage.unusedProducts === Map(Glass -> 16000, Fruit -> 24000, MachineParts -> 1000))

    val moneySpentOnResources = 16000 * 10 + 24000 * 5 + 1000
    val moneyEarned = 40000 * 100

    assert(factory.currentMoneyBalance === 1000000 - moneySpentOnResources)
    val moneyBalanceAfterDemand = factory.currentMoneyBalance
    val profit = Map(region1 -> FulfilledSupplyRequestProfit(
      FulfilledSupplyRequest(40000, 0, 200, sell.head._2), 100))
    factory.receiveSellingResultAndMoney(profit)
    assert(factory.factoryStorage.unsoldProducts === 0)
    assert(factory.currentMoneyBalance === moneyBalanceAfterDemand + moneyEarned)

    val owners = new Population(LatinHuman, Capitalists, 1000, 0, 0)

    assert(factory.workforceEfficiencyDemand(prices) === 2000)

    val workers = new Population(LatinHuman, Craftsmen, 10000, 0, 0)
    assert(workers.efficiency === 1)

    factory.receiveWorkforceRequest(Map(workers -> 2000))

    factory.payMoneyToPops(List(owners))
    val taxes = (moneyEarned - moneySpentOnResources) * 0.1
    assert(factory.payTaxes() === taxes)

    assert(workers.moneyReserves === (moneyEarned - moneySpentOnResources) * 0.9 * 0.4)

    factory.produce()

    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")
    assert(factory.factoryStorage.unsoldProducts === 40000)

    factory.endOfDay()

    assert(factory.dayRecords === Vector(FactoryRecord(
      produced = 40000,
      moneySpentOnResources = moneySpentOnResources,
      moneyOnWorkforceSalary = (moneyEarned - moneySpentOnResources) * 0.9 * 0.4,
      moneyOnOwnersPayment = (moneyEarned - moneySpentOnResources) * 0.9 * 0.4,
      corporateTax = taxes,
      moneyLeftInFactoryBudget = (moneyEarned - moneySpentOnResources) * 0.9 * 0.2,
      peopleResources = Map(workers -> 2000),
      sold = profit
    )))
  }

  test("factory usage general flow - deficit path, half workers are slaves") {
    val factory = new IndustrialFactory(Wine, 2, 2000)
    val prices = Map[Product, Double](Glass -> 1, Fruit -> 1, MachineParts -> 1)

    factory.newDay(FactoryTaxPolicy(0.1))
    assert(factory.dayRecords === Vector())
    val requests = factory.componentDemandRequests(prices)
    assert(requests.mapValues(_.count) === Map(Glass -> 400, Fruit -> 600, MachineParts -> 1000))

    assert(factory.factoryStorage.unsoldProducts === 40000)
    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")

    val sell = factory.sellProduct(Map(region1 -> EconomicRegionDemand(1000, 10),
      region2 -> EconomicRegionDemand(1000, 5)))
    assert(sell.mapValues(_.count) === Map(region1 -> 20000, region2 -> 20000))

    val fulfilledRequests = requests.transform { case (p, r) =>
      FulfilledDemandRequest(r.count / 2 , r.count / 2, prices(p), r)
    }

    assert(factory.currentMoneyBalance === 2000)
    factory.receiveFulfilledDemandRequestsAndPayChecks(fulfilledRequests)
    assert(factory.factoryStorage.unusedProducts === Map(Glass -> 200, Fruit -> 300, MachineParts -> 500))

    val moneySpentOnResources = 1000
    val moneyEarned = 100

    val profit = Map(region1 -> FulfilledSupplyRequestProfit(FulfilledSupplyRequest(50, 950, 200, sell.head._2), 2),
      region2 -> FulfilledSupplyRequestProfit(FulfilledSupplyRequest(0, 1000, 200, sell.head._2), 5))
    factory.receiveSellingResultAndMoney(profit)
    assert(factory.factoryStorage.unsoldProducts === 39950)
    assert(factory.currentMoneyBalance === 2000 - moneySpentOnResources + moneyEarned)

    val owners = new Population(LatinHuman, Capitalists, 1000, 0, 0)

    assert(factory.workforceEfficiencyDemand(prices) === 2.5)

    val workers = new Population(LatinHuman, Craftsmen, 10000, 0, 0)
    assert(workers.efficiency === 1)
    val slaves = new Population(LatinHuman, Slaves, 10000, 0, 0)
    assert(slaves.efficiency === 1)

    factory.receiveWorkforceRequest(Map(workers -> 1.25, slaves -> 1.25))

    factory.payMoneyToPops(List(owners))
    val taxes = 0
    assert(factory.payTaxes() === taxes)

    assert(factory.factoryStorage.unsoldProducts === 39950)
    factory.produce()

    assert(factory.factoryStorage.unusedProducts === Map(Glass -> 183.0, Fruit -> 274.5, MachineParts -> 0.0))
    assert(factory.factoryStorage.unsoldProducts === 39992.5)

    factory.endOfDay()

    assert(factory.dayRecords === Vector(FactoryRecord(
      produced = 42.5,
      moneySpentOnResources = moneySpentOnResources,
      moneyOnWorkforceSalary = 0,
      moneyOnOwnersPayment = 0,
      corporateTax = taxes,
      moneyLeftInFactoryBudget = 0,
      peopleResources = Map(workers -> 1.25, slaves -> 1.25),
      sold = profit
    )))
  }

  test("factory usage general flow - happy path, half workers are slaves") {
    val factory = new IndustrialFactory(Wine, 2, 1000000)

    val prices = Map[Product, Double](Glass -> 10, Fruit -> 5, MachineParts -> 1)

    factory.newDay(FactoryTaxPolicy(0.1))
    assert(factory.dayRecords === Vector())
    val requests = factory.componentDemandRequests(prices)
    assert(requests.mapValues(_.count) === Map(Glass -> 16000, Fruit -> 24000, MachineParts -> 1000))

    assert(factory.factoryStorage.unsoldProducts === 40000)
    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")

    val sell = factory.sellProduct(Map(region1 -> EconomicRegionDemand(40000, 100),
      region2 -> EconomicRegionDemand(40000, 50)))
    assert(sell.mapValues(_.count) === Map(region1 -> 40000))

    val fulfilledRequests = requests.transform { case (p, r) =>
      FulfilledDemandRequest(r.count, 0, prices(p), r)
    }

    assert(factory.currentMoneyBalance === 1000000)
    factory.receiveFulfilledDemandRequestsAndPayChecks(fulfilledRequests)
    assert(factory.factoryStorage.unusedProducts === Map(Glass -> 16000, Fruit -> 24000, MachineParts -> 1000))

    val moneySpentOnResources = 16000 * 10 + 24000 * 5 + 1000
    val moneyEarned = 40000 * 100

    assert(factory.currentMoneyBalance === 1000000 - moneySpentOnResources)
    val moneyBalanceAfterDemand = factory.currentMoneyBalance
    val profit = Map(region1 -> FulfilledSupplyRequestProfit(
      FulfilledSupplyRequest(40000, 0, 200, sell.head._2), 100))
    factory.receiveSellingResultAndMoney(profit)
    assert(factory.factoryStorage.unsoldProducts === 0)
    assert(factory.currentMoneyBalance === moneyBalanceAfterDemand + moneyEarned)

    val owners = new Population(LatinHuman, Capitalists, 1000, 0, 0)

    assert(factory.workforceEfficiencyDemand(prices) === 2000)

    val workers = new Population(LatinHuman, Craftsmen, 10000, 0, 0)
    assert(workers.efficiency === 1)
    val slaves = new Population(LatinHuman, Slaves, 10000, 0, 0)
    assert(slaves.efficiency === 1)

    factory.receiveWorkforceRequest(Map(workers -> 1000, slaves -> 1000))

    factory.payMoneyToPops(List(owners))
    val taxes = (moneyEarned - moneySpentOnResources) * 0.1
    assert(factory.payTaxes() === taxes)

    assert(workers.moneyReserves === (moneyEarned - moneySpentOnResources) * 0.9 * 0.4 * 0.5)

    factory.produce()

    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")
    assert(factory.factoryStorage.unsoldProducts === 40000)

    factory.endOfDay()

    assert(factory.dayRecords === Vector(FactoryRecord(
      produced = 40000,
      moneySpentOnResources = moneySpentOnResources,
      moneyOnWorkforceSalary = (moneyEarned - moneySpentOnResources) * 0.9 * 0.4 * 0.5,
      moneyOnOwnersPayment = (moneyEarned - moneySpentOnResources) * 0.9 * 0.4,
      corporateTax = taxes,
      moneyLeftInFactoryBudget = (moneyEarned - moneySpentOnResources) * 0.9 * (0.2 + 0.4 * 0.5),
      peopleResources = Map(workers -> 1000, slaves -> 1000),
      sold = profit
    )))
  }

  test("factory usage general flow - happy path, all workers are slaves") {
    val factory = new IndustrialFactory(Wine, 2, 1000000)

    val prices = Map[Product, Double](Glass -> 10, Fruit -> 5, MachineParts -> 1)

    factory.newDay(FactoryTaxPolicy(0.1))
    assert(factory.dayRecords === Vector())
    val requests = factory.componentDemandRequests(prices)
    assert(requests.mapValues(_.count) === Map(Glass -> 16000, Fruit -> 24000, MachineParts -> 1000))

    assert(factory.factoryStorage.unsoldProducts === 40000)
    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")

    val sell = factory.sellProduct(Map(region1 -> EconomicRegionDemand(40000, 100),
      region2 -> EconomicRegionDemand(40000, 50)))
    assert(sell.mapValues(_.count) === Map(region1 -> 40000))

    val fulfilledRequests = requests.transform { case (p, r) =>
      FulfilledDemandRequest(r.count, 0, prices(p), r)
    }

    assert(factory.currentMoneyBalance === 1000000)
    factory.receiveFulfilledDemandRequestsAndPayChecks(fulfilledRequests)
    assert(factory.factoryStorage.unusedProducts === Map(Glass -> 16000, Fruit -> 24000, MachineParts -> 1000))

    val moneySpentOnResources = 16000 * 10 + 24000 * 5 + 1000
    val moneyEarned = 40000 * 100

    assert(factory.currentMoneyBalance === 1000000 - moneySpentOnResources)
    val moneyBalanceAfterDemand = factory.currentMoneyBalance
    val profit = Map(region1 -> FulfilledSupplyRequestProfit(
      FulfilledSupplyRequest(40000, 0, 200, sell.head._2), 100))
    factory.receiveSellingResultAndMoney(profit)
    assert(factory.factoryStorage.unsoldProducts === 0)
    assert(factory.currentMoneyBalance === moneyBalanceAfterDemand + moneyEarned)

    val owners = new Population(LatinHuman, Capitalists, 1000, 0, 0)

    assert(factory.workforceEfficiencyDemand(prices) === 2000)

    val slaves = new Population(LatinHuman, Slaves, 10000, 0, 0)
    assert(slaves.efficiency === 1)

    factory.receiveWorkforceRequest(Map(slaves -> 2000))

    factory.payMoneyToPops(List(owners))
    val taxes = (moneyEarned - moneySpentOnResources) * 0.1
    assert(factory.payTaxes() === taxes)

    factory.produce()

    assert(factory.factoryStorage.unusedProducts.forall(_._2 == 0),
      s"${factory.factoryStorage.unusedProducts} must be empty")
    assert(factory.factoryStorage.unsoldProducts === 40000)

    factory.endOfDay()

    assert(factory.dayRecords === Vector(FactoryRecord(
      produced = 40000,
      moneySpentOnResources = moneySpentOnResources,
      moneyOnWorkforceSalary = 0,
      moneyOnOwnersPayment = (moneyEarned - moneySpentOnResources) * 0.9 * 0.4,
      corporateTax = taxes,
      moneyLeftInFactoryBudget = (moneyEarned - moneySpentOnResources) * 0.9 * (0.2 + 0.4),
      peopleResources = Map(slaves -> 2000),
      sold = profit
    )))
  }
}
