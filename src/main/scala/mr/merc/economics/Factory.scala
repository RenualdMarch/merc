package mr.merc.economics

import Products._
import mr.merc.economics.Factory.{FactoryRecord, FactoryStorage}
import mr.merc.economics.Population._
import mr.merc.economics.MapUtil.MapWithOperations

object Factory {
  case class FactoryStorage(unsoldProducts: Double, money: Double, unusedProducts: Map[Product, Double])

  case class FactoryRecord(produced: Double, moneySpentOnResources: Double, moneyOnWorkforceSalary: Double,
                           moneyOnOwnersPayment: Double, corporateTax: Double, moneyToFactoryBudget: Double,
                           peopleResources:Map[Population, Double], sold: Map[EconomicRegion, FulfilledSupplyRequestProfit]) {
    def earnings: Double = sold.values.map(p => p.profitPerItem * p.request.sold).sum
    def itemsSold: Double = sold.values.map(_.request.sold).sum
    def factoryBuySellProfit: Double = earnings - moneySpentOnResources
  }
}

abstract class Factory[Producible <: ProducibleProduct](val region: EconomicRegion, val product: Producible, startingMoney: Double,
                                                        startingProducts: Double, inputMultiplier: Double, outputMultiplier: Double) extends Enterprise {

  private val FactoryRecordsMax = 30

  private val ProfitPartToWorkers = 0.5
  private val ProfitPartToOwners = 0.5
  // TODO move money to storage smartly

  private val noMaintenancePenalty = 0.3

  private var currentTaxPolicy:CorporateTaxPolicy = _

  private var storage = FactoryStorage(unsoldProducts = startingProducts, money = startingMoney, unusedProducts = Map())

  private var factoryRecords:Vector[FactoryRecord] = Vector()

  def isResourceGathering: Boolean = false

  def dayRecords:Vector[FactoryRecord] = factoryRecords

  private var currentRecord = newFactoryRecord()

  private val supplyDecider = new SupplyDecider()

  protected def requiredMaintenance:Map[Product, Double]

  protected def maxPossibleInputToUse:Double

  private def costOfOneOutput(prices:Map[Product, Double]): Double = product.components.map { case (p, count) => prices(p) * count }.sum

  def factoryStorage: FactoryStorage = storage

  private [economics] def maxMoneyReserves: Double = {
    val neededForProduction = product.components.mapValues(_ * maxPossibleInputToUse)
    val neededForMaintenance = requiredMaintenance
    val maxNeededForDay = neededForProduction |+| neededForMaintenance
    maxNeededForDay.map { case (p, c) =>
      region.regionMarket.currentPrices(p) * c
    }.sum
  }

  private def capacity(prices:Map[Product, Double]): Double = {
    val maintenancePrice = requiredMaintenance.toList.map { case (p, count) =>
      count * prices(p)
    }.sum
    val possibleToBuy = (storage.money - maintenancePrice) / costOfOneOutput(prices)
    scala.math.min(possibleToBuy, maxPossibleInputToUse)
  }

  def componentDemandRequests(prices:Map[Product, Double]):Map[Product, DemandRequest] = {
    val targetProduction = supplyDecider.decideProduction(capacity(prices))
    val requiredComponents = product.components.mapValues(_ * targetProduction)
    val demand = (requiredComponents |-| storage.unusedProducts).filter(_._2 > 0)
    (demand |+| requiredMaintenance).transform{case (p, c) => EnterpriseDemandRequest(this, p, c)}
  }

  def workforceEfficiencyDemand(prices:Map[Product, Double]):Double = {
    val targetProduction = supplyDecider.decideProduction(capacity(prices))
    val neededEfficiency = targetProduction / inputMultiplier
    val maxEfficiency = maxPossibleInputToUse / inputMultiplier
    scala.math.min(neededEfficiency, maxEfficiency)
  }

  def receiveFulfilledDemandRequestsAndPayChecks(requests:Map[Product, FulfilledDemandRequest]): Unit = {
    var moneySpentOnResources = 0d
    requests.foreach { case (p, r) =>
      moneySpentOnResources = moneySpentOnResources + r.price * r.bought
      val components = storage.unusedProducts.getOrElse(p, 0d) + r.bought
      storage = storage.copy(unusedProducts = storage.unusedProducts + (p -> components))
    }
    storage = storage.copy(money = storage.money - moneySpentOnResources)
    currentRecord = currentRecord.copy(moneySpentOnResources = currentRecord.moneySpentOnResources + moneySpentOnResources)
  }

  def receiveWorkforceRequest(result: Map[Population, Double]): Unit = {
    require(result.keys.forall(p => possibleWorkers == p.populationType), s"received workforce $result but possible types are $possibleWorkers")
    currentRecord = currentRecord.copy(peopleResources = result)
  }

  def produce(): Unit = {
    val minFromProducts = product.components.map { case (p, d) =>
      storage.unusedProducts.getOrElse(p, 0d) / d
    }.min

    val workforceCanProduce = currentRecord.peopleResources.map{case (pop, count) =>
      pop.efficiency * count
    }.sum

    val notEnoughMaintenance = (storage.unusedProducts |-| requiredMaintenance).filter(_._2 < 0)
    val notEnoughMaintenanceItems = notEnoughMaintenance.values.sum * (-1)
    val notEnoughMaintenancePercentage = notEnoughMaintenanceItems / requiredMaintenance.values.sum
    val notEnoughMaintenancePenalty = notEnoughMaintenancePercentage * noMaintenancePenalty

    val fromPeople = scala.math.min(workforceCanProduce * inputMultiplier, maxPossibleInputToUse)
    val produce = scala.math.min(minFromProducts, fromPeople) * (1 - notEnoughMaintenancePenalty) * outputMultiplier
    val spentResources = (product.components |*| produce) |+| (requiredMaintenance |+| notEnoughMaintenance)
    storage = storage.copy(unusedProducts = storage.unusedProducts |-| spentResources)
    currentRecord = currentRecord.copy(produced = produce)
    storage = storage.copy(unsoldProducts = storage.unsoldProducts + produce)
  }

  def sellProduct(demand:Map[EconomicRegion, EconomicRegionDemand]):Map[EconomicRegion, SupplyRequest] = {
    supplyDecider.decideSupply(storage.unsoldProducts, demand).collect { case (r, supply) if supply > 0 =>
      r -> EnterpriseSupplyRequest(this, product, supply)
    }
  }

  def receiveSellingResultAndMoney(region:EconomicRegion, profit:FulfilledSupplyRequestProfit): Unit = {
    supplyDecider.receiveSupplyResults(Map(region -> profit.request))
    currentRecord = currentRecord.copy(sold = currentRecord.sold + (region -> profit))
    storage = storage.copy(
      money = storage.money + profit.request.sold * profit.profitPerItem,
      unsoldProducts = storage.unsoldProducts - profit.request.sold
    )
  }

  private def efficiency(pop:Population, workers: Double):Double = pop.efficiency * workers

  private def calculateEarningsDistribution(): Unit = {
    val tax = currentRecord.factoryBuySellProfit * currentTaxPolicy.corporateTax
    if (tax < 0) {
      currentRecord = currentRecord.copy(
        corporateTax = 0,
        moneyOnWorkforceSalary = 0,
        moneyOnOwnersPayment = 0,
        moneyToFactoryBudget = 0
      )
    } else {
      val moneyToPeople = storage.money - tax - maxMoneyReserves
      if (moneyToPeople < 0) {
        // then all money goes to storage, no salary and no dividents
        currentRecord = currentRecord.copy(
          corporateTax = tax,
          moneyOnWorkforceSalary = 0,
          moneyOnOwnersPayment = 0,
          moneyToFactoryBudget = storage.money - tax
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
          corporateTax = currentRecord.factoryBuySellProfit * currentTaxPolicy.corporateTax,
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
        val money = currentRecord.moneyOnWorkforceSalary / totalEfficiency * efficiency(pop, count)
        pop.receiveSalary(money)
      }
    }

    storage = storage.copy(money = storage.money - currentRecord.moneyOnOwnersPayment - currentRecord.moneyOnWorkforceSalary)
  }

  def payTaxes(): Double = {
    storage = storage.copy(money = storage.money - currentRecord.corporateTax)
    currentRecord.corporateTax
  }

  def endOfDay():Unit = {
    factoryRecords :+= currentRecord
    if (factoryRecords.size > FactoryRecordsMax) {
      factoryRecords = factoryRecords.takeRight(FactoryRecordsMax)
    }
  }

  def newDay(taxPolicy: CorporateTaxPolicy): Unit = {
    currentTaxPolicy = taxPolicy
    currentRecord = newFactoryRecord()
  }

  val possibleWorkers: PopulationType

  def currentMoneyBalance:Double = storage.money

  def owners: List[Population]

  private def newFactoryRecord() = FactoryRecord(0,0, 0, 0, 0, 0, Map(), Map())

  override def expectedSalaryPerEfficiency: Double = {
    dayRecords.headOption match {
      case Some(c) =>
        val money = c.moneyOnWorkforceSalary
        val workersEfficiency = c.peopleResources.map{
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

  private val maintenanceGoodsPerLevel:Map[Product, Double] = Map(MachineParts -> 500)

  override protected def requiredMaintenance:Map[Product, Double] = maintenanceGoodsPerLevel |*| level

  override protected def maxPossibleInputToUse:Double = level * Products.PeoplePerOneFactoryLevel * inputMultiplier

  override def owners: List[Population] = region.regionPopulation.pops.filter(_.populationType == Capitalists)

}

// implement industrial factory as factory without level, limited only by worker presence
class MagicGuildEnterprise(region: EconomicRegion, product: MagicProduct, startingMoney: Double, startingProducts: Double, inputMultiplier: Double, outputMultiplier: Double) extends
  Factory[MagicProduct](region, product, startingMoney, startingProducts, inputMultiplier, outputMultiplier) {

  override protected def requiredMaintenance:Map[Product, Double] = Map()

  override val possibleWorkers: PopulationType = Mages

  override protected def maxPossibleInputToUse:Double = region.regionPopulation.getPopTotalEfficiency(Mages) * inputMultiplier

  override def owners: List[Population] = region.regionPopulation.pops.filter(_.populationType == Mages)
}