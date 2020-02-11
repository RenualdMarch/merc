package mr.merc.economics

import Products._
import mr.merc.economics.Factory.{FactoryRecord, FactoryStorage}
import mr.merc.economics.Population.{PopulationType, _}
import mr.merc.economics.MapUtil.FloatOperations._
import mr.merc.economics.TaxPolicy.CorporateTax
import WorldConstants.Enterprises._

object Factory {

  case class FactoryStorage(unsoldProducts: Double, money: Double, unusedProducts: Map[Product, Double])

  case class FactoryRecord(produced: Double, moneySpentOnResources: Double, moneyOnWorkforceSalary: Double,
                           moneyOnOwnersPayment: Double, corporateTax: Double, moneyToFactoryBudget: Double,
                           peopleResources: Map[Population, Double], bought: List[FulfilledDemandRequest],
                           sold: Map[EconomicRegion, FulfilledSupplyRequestProfit], turn: Int) extends DayRecord {
    def factoryBuySellProfit: Double = earnings - moneySpentOnResources
  }

}

abstract class Factory[Producible <: IndustryProduct](val region: EconomicRegion, val product: Producible, startingMoney: Double,
                                                        startingProducts: Double, inputMultiplier: Double, outputMultiplier: Double) extends Enterprise {

  private val FactoryRecordsMax = 30

  private var currentTaxPolicy: Double = _

  protected var storage = FactoryStorage(unsoldProducts = startingProducts, money = startingMoney, unusedProducts = Map())

  private var factoryRecords: Vector[FactoryRecord] = Vector()

  def isResourceGathering: Boolean = false

  def dayRecords: Vector[FactoryRecord] = factoryRecords

  private var currentRecord:FactoryRecord = _

  private val supplyDecider = new SupplyDecider()

  def maxPossibleInputToUse: Double

  private def costOfOneOutput(prices: Map[Product, Double]): Double = product.components.map { case (p, count) => prices(p) * count }.sum

  def factoryStorage: FactoryStorage = storage

  def isBankrupt:Boolean = {
    storage.unsoldProducts <= BankruptStorage && storage.money <= BankruptMoney
  }

  private[economics] def maxMoneyReserves: Double = {
    val neededForProduction = product.components.mapValues(_ * maxPossibleInputToUse)
    neededForProduction.map { case (p, c) =>
      region.regionMarket.currentPrices(p) * c
    }.sum
  }

  private def capacity(prices: Map[Product, Double]): Double = {
    val possibleToBuy = storage.money / costOfOneOutput(prices)
    scala.math.min(possibleToBuy, maxPossibleInputToUse)
  }

  def componentDemandRequests(prices: Map[Product, Double]): Map[Product, DemandRequest] = {
    val targetProduction = supplyDecider.decideProduction(capacity(prices), storage.unsoldProducts)
    val requiredComponents = product.components.mapValues(_ * targetProduction)
    val demand = (requiredComponents |-| storage.unusedProducts).filter(_._2 > 0)
    demand.transform { case (p, c) => EnterpriseDemandRequest(this, p, c) }
  }

  def workforceEfficiencyDemand(prices: Map[Product, Double]): Double = {
    val targetProduction = supplyDecider.decideProduction(capacity(prices), storage.unsoldProducts)
    val neededEfficiency = targetProduction / inputMultiplier
    val maxEfficiency = maxPossibleInputToUse / inputMultiplier
    scala.math.min(neededEfficiency, maxEfficiency)
  }

  def buyDemandedProducts(requests: List[FulfilledDemandRequest]): Unit = {
    var moneySpentOnResources = 0d
    requests.foreach { r =>
      val product = r.request.product
      moneySpentOnResources = moneySpentOnResources + r.spentMoney
      r.currentSpentMoney += r.spentMoney
      val components = storage.unusedProducts.getOrElse(product, 0d) + r.bought
      storage = storage.copy(unusedProducts = storage.unusedProducts + (product -> components))
    }
    storage = storage.copy(money = storage.money - moneySpentOnResources)
    currentRecord = currentRecord.copy(
      moneySpentOnResources = currentRecord.moneySpentOnResources + moneySpentOnResources,
      bought = requests ::: currentRecord.bought)
  }

  def receiveWorkforceRequest(result: Map[Population, Double]): Unit = {
    require(result.keys.forall(p => possibleWorkers == p.populationType), s"received workforce $result but possible types are $possibleWorkers")
    currentRecord = currentRecord.copy(peopleResources = result)
  }

  def produce(): Unit = {
    val minFromProducts = product.components.map { case (p, d) =>
      storage.unusedProducts.getOrElse(p, 0d) / d
    }.min

    val workforceCanProduce = currentRecord.peopleResources.map { case (pop, count) =>
      pop.efficiency * count
    }.sum

    val fromPeople = scala.math.min(workforceCanProduce * inputMultiplier, maxPossibleInputToUse)
    val produce = scala.math.min(minFromProducts, fromPeople)
    val spentResources = product.components |*| produce
    storage = storage.copy(unusedProducts = storage.unusedProducts |-| spentResources)
    val actualProduce = produce * outputMultiplier
    currentRecord = currentRecord.copy(produced = actualProduce)
    storage = storage.copy(unsoldProducts = storage.unsoldProducts + actualProduce)
  }

  def sellProduct(demand: Map[EconomicRegion, EconomicRegionDemand]): Map[EconomicRegion, SupplyRequest] = {
    supplyDecider.decideSupply(storage.unsoldProducts, demand).collect { case (r, supply) if supply > 0 =>
      r -> EnterpriseSupplyRequest(this, product, supply)
    }
  }

  def receiveSellingResultAndMoney(region: EconomicRegion, profit: FulfilledSupplyRequestProfit): Unit = {
    currentRecord = currentRecord.copy(sold = currentRecord.sold + (region -> profit))
    val receivedMoney = profit.request.sold * profit.profitPerItem
    storage = storage.copy(
      money = storage.money + receivedMoney,
      unsoldProducts = storage.unsoldProducts - profit.request.sold
    )
    profit.request.currentSpentMoney -= receivedMoney
  }

  private def efficiency(pop: Population, workers: Double): Double = pop.efficiency * workers

  private def calculateEarningsDistribution(): Unit = {
    val tax = currentRecord.factoryBuySellProfit * currentTaxPolicy
    if (tax < 0) {
      currentRecord = currentRecord.copy(
        corporateTax = 0,
        moneyOnWorkforceSalary = 0,
        moneyOnOwnersPayment = 0,
        moneyToFactoryBudget = currentRecord.factoryBuySellProfit
      )
    } else {
      val moneyToPeople = storage.money - tax - maxMoneyReserves
      if (moneyToPeople < 0) {
        // then all money goes to storage, no salary and no dividents
        currentRecord = currentRecord.copy(
          corporateTax = tax,
          moneyOnWorkforceSalary = 0,
          moneyOnOwnersPayment = 0,
          moneyToFactoryBudget = currentRecord.factoryBuySellProfit - tax
        )
      } else if (currentRecord.peopleResources.values.sum == 0) {
        currentRecord = currentRecord.copy(
          corporateTax = tax,
          moneyOnWorkforceSalary = 0,
          moneyOnOwnersPayment = moneyToPeople,
          moneyToFactoryBudget = maxMoneyReserves
        )
      } else {
        currentRecord = currentRecord.copy(
          corporateTax = currentRecord.factoryBuySellProfit * currentTaxPolicy,
          moneyOnWorkforceSalary = moneyToPeople * ProfitPartToWorkers,
          moneyOnOwnersPayment = moneyToPeople * ProfitPartToOwners,
          moneyToFactoryBudget = maxMoneyReserves
        )
      }
    }
  }

  def payMoneyToPops(): Unit = {
    calculateEarningsDistribution()

    if (owners.map(_.populationCount).sum == 0) {
      // exceptional case
      owners.head.receiveSalary(currentRecord.moneyOnOwnersPayment)
    } else {
      paySalaryProportionallyToEfficiency(owners.map(p => p -> p.populationCount.toDouble).toMap, currentRecord.moneyOnOwnersPayment)
    }

    val totalEfficiency: Double = currentRecord.peopleResources.toList.map(efficiency _ tupled).sum

    if (totalEfficiency != 0) {
      currentRecord.peopleResources.foreach { case (pop, count) =>
        val popMoney = currentRecord.moneyOnWorkforceSalary / totalEfficiency * efficiency(pop, count)
        pop.receiveSalary(popMoney)
      }
    } else {
      val pops = region.regionPopulation.popsByType(possibleWorkers)
      if (pops.map(_.populationCount).sum != 0) {
        paySalaryProportionallyToEfficiency(pops.map(p => p -> p.populationCount.toDouble).toMap, currentRecord.moneyOnWorkforceSalary)
      } else {
        pops.head.receiveSalary(currentRecord.moneyOnWorkforceSalary)
      }
    }

    storage = storage.copy(money = storage.money - currentRecord.moneyOnOwnersPayment - currentRecord.moneyOnWorkforceSalary)
  }

  def payTaxes(): Unit = {
    storage = storage.copy(money = storage.money - currentRecord.corporateTax)
    val gross = if (currentRecord.factoryBuySellProfit > 0) currentRecord.factoryBuySellProfit else 0d
    region.owner.budget.receiveTaxes(TaxData(CorporateTax, gross, currentRecord.corporateTax))
  }

  def endOfDay(): Unit = {
    factoryRecords :+= currentRecord
    if (factoryRecords.size > FactoryRecordsMax) {
      factoryRecords = factoryRecords.takeRight(FactoryRecordsMax)
    }
  }

  def newDay(taxPolicy: TaxPolicy, bureaucratsPercentage: Double, turn: Int): Unit = {
    currentTaxPolicy = taxPolicy.tax(CorporateTax, bureaucratsPercentage)
    currentRecord = newFactoryRecord(turn)
  }

  val possibleWorkers: PopulationType

  def currentMoneyBalance: Double = storage.money

  def unsoldProducts: Double = storage.unsoldProducts

  def owners: List[Population]

  private def newFactoryRecord(turn: Int) = FactoryRecord(0, 0, 0, 0, 0, 0, Map(), Nil, Map(), turn)

  override def expectedSalaryPerEfficiency: Double = {
    dayRecords.headOption match {
      case Some(c) =>
        val money = c.moneyOnWorkforceSalary
        val workersEfficiency = c.peopleResources.map {
          case (p, count) => p.efficiency * count
        }.sum
        if (workersEfficiency != 0) {
          money / workersEfficiency
        } else money
      case None => 1d
    }
  }
}

case class FulfilledSupplyRequestProfit(request: FulfilledSupplyRequest, profitPerItem: Double)

class IndustrialFactory(region: EconomicRegion, product: IndustryProduct, var level: Int, startingMoney: Double, startingProducts: Double, inputMultiplier: Double, outputMultiplier: Double) extends
  Factory[IndustryProduct](region, product, startingMoney, startingProducts, inputMultiplier, outputMultiplier) {

  override val possibleWorkers: PopulationType = Craftsmen

  override def maxPossibleInputToUse: Double = level * EfficiencyPerOneFactoryLevel * inputMultiplier

  def decreaseLevel(): Unit = {
    require(level > 1, s"Level is $level, must be greater than 1")
    level = level - 1
    storage = storage.copy(unsoldProducts = factoryStorage.unsoldProducts + maxPossibleInputToUse * outputMultiplier)
  }


  override def owners: List[Population] = region.regionPopulation.pops.filter(_.populationType == Capitalists)
}