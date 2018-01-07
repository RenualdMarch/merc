package mr.merc.economics

import Products._
import mr.merc.economics.Factory.{FactoryRecord, FactoryStorage}
import mr.merc.economics.Population.{Craftsmen, Mages, PopulationType, Slaves}
import mr.merc.economics.MapUtil.MapWithOperations

object Factory {
  case class FactoryStorage(unsoldProducts: Double, money: Double, unusedProducts: Map[Product, Double])

  case class FactoryRecord(produced: Double, moneySpentOnResources: Double, moneyOnWorkforceSalary: Double,
                           moneyOnOwnersPayment: Double, corporateTax: Double, moneyLeftInFactoryBudget: Double,
                           peopleResources:Map[Population, Double], sold: Map[EconomicRegion, FulfilledSupplyRequestProfit]) {
    def earnings: Double = sold.values.map(p => p.profitPerItem * p.request.sold).sum
    def itemsSold: Double = sold.values.map(_.request.sold).sum
    def factoryBuySellProfit: Double = earnings - moneySpentOnResources
  }
}

abstract class Factory[Producible <: ProducibleProduct](val product: Producible, var level: Int, startingMoney: Double) {

  private val FactoryRecordsMax = 30

  private val ProfitPartToWorkers = 0.4
  private val ProfitPartToOwners = 0.4
  private val ProfitPartToStorage = 0.2

  private val initialUnsoldProductsQ = 1

  private val noMaintenancePenalty = 0.3

  private var currentTaxPolicy:FactoryTaxPolicy = FactoryTaxPolicy(0)

  private var storage = FactoryStorage(unsoldProducts = maxPossibleToProduce * initialUnsoldProductsQ, money = startingMoney, unusedProducts = Map())

  private var factoryRecords:Vector[FactoryRecord] = Vector()

  def dayRecords:Vector[FactoryRecord] = factoryRecords

  private var currentRecord = newFactoryRecord()

  private val supplyDecider = new SupplyDecider()

  private val maintenanceGoodsPerLevel:Map[Product, Double] = Map(MachineParts -> 500)
  private def requiredMaintenance:Map[Product, Double] = maintenanceGoodsPerLevel |*| level

  private def maxPossibleToProduce:Double = level * Products.PeoplePerOneFactoryLevel * Products.FactoryOutputMultiplier

  private def costOfOneOutput(prices:Map[Product, Double]): Double = product.components.map { case (p, count) => prices(p) * count }.sum

  def factoryStorage: FactoryStorage = storage

  private def capacity(prices:Map[Product, Double]): Double = {
    val maintenancePrice = requiredMaintenance.toList.map { case (p, count) =>
      count * prices(p)
    }.sum
    val possibleToBuy = (storage.money - maintenancePrice) / costOfOneOutput(prices)
    scala.math.min(possibleToBuy, maxPossibleToProduce)
  }

  def componentDemandRequests(prices:Map[Product, Double]):Map[Product, DemandRequest] = {
    val targetProduction = supplyDecider.decideProduction(capacity(prices))
    val requiredComponents = product.components.mapValues(_ * targetProduction)
    val demand = (requiredComponents |-| storage.unusedProducts).filter(_._2 > 0)
    (demand |+| requiredMaintenance).transform{case (_, c) => new DemandRequest(c)}
  }

  def workforceEfficiencyDemand(prices:Map[Product, Double]):Double = {
    val targetProduction = supplyDecider.decideProduction(capacity(prices))
    val neededEfficiency = targetProduction / Products.FactoryOutputMultiplier
    val maxEfficiency = Products.PeoplePerOneFactoryLevel * level
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
    currentRecord = currentRecord.copy(moneySpentOnResources = moneySpentOnResources)
  }

  def receiveWorkforceRequest(result: Map[Population, Double]): Unit = {
    require(result.keys.forall(p => possibleWorkers.contains(p.populationType)), s"received workforce $result but possible types are $possibleWorkers")
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

    val fromPeople = scala.math.min(workforceCanProduce * Products.FactoryOutputMultiplier, maxPossibleToProduce)
    val produce = scala.math.min(minFromProducts, fromPeople) * (1 - notEnoughMaintenancePenalty)
    val spentResources = (product.components |*| produce) |+| (requiredMaintenance |+| notEnoughMaintenance)
    storage = storage.copy(unusedProducts = storage.unusedProducts |-| spentResources)
    currentRecord = currentRecord.copy(produced = produce)
    storage = storage.copy(unsoldProducts = storage.unsoldProducts + produce)

  }

  def sellProduct(demand:Map[EconomicRegion, EconomicRegionDemand]):Map[EconomicRegion, SupplyRequest] = {
    supplyDecider.decideSupply(storage.unsoldProducts, demand).collect { case (r, supply) if supply > 0 =>
      r -> new SupplyRequest(supply)
    }
  }

  def receiveSellingResultAndMoney(result: Map[EconomicRegion, FulfilledSupplyRequestProfit]): Unit = {
    supplyDecider.receiveSupplyResults(result.mapValues(_.request))
    currentRecord = currentRecord.copy(sold = result)
    storage = storage.copy(
      money = storage.money + currentRecord.earnings,
      unsoldProducts = storage.unsoldProducts - currentRecord.sold.values.map(_.request.sold).sum
    )
  }

  private def efficiency(pop:Population, workers: Double):Double = pop.efficiency * workers

  private def calculateEarningsDistribution(): Unit = {
    val slavePartInWorkforce: Double = {
      val slavesPops = currentRecord.peopleResources.filter(_._1.populationType == Slaves)
      val totalEfficiency = currentRecord.peopleResources.toList.map(efficiency _ tupled).sum
      if (totalEfficiency > 0) {
        slavesPops.toList.map(efficiency _ tupled).sum / totalEfficiency
      } else 0
    }

    val moneyToShare = currentRecord.factoryBuySellProfit * (1 - currentTaxPolicy.corporateTax)
    if (moneyToShare < 0) {
      return
    } //do nothing because all values are already zero

    currentRecord = currentRecord.copy(
      corporateTax = currentRecord.factoryBuySellProfit * currentTaxPolicy.corporateTax,
      moneyOnWorkforceSalary = moneyToShare * ProfitPartToWorkers * (1 - slavePartInWorkforce),
      moneyOnOwnersPayment = moneyToShare * ProfitPartToOwners,
      moneyLeftInFactoryBudget = moneyToShare * (ProfitPartToStorage + ProfitPartToWorkers * slavePartInWorkforce)
    )
  }

  // TODO factory should know its owners
  def payMoneyToPops(owners: List[Population]): Unit = {
    calculateEarningsDistribution()

    if (owners.map(_.populationCount).sum == 0) {
      // exceptional case
      owners.head.receiveSalary(currentRecord.moneyOnOwnersPayment)
    } else {
      val totalPopulation = owners.map(_.populationCount).sum.toDouble
      owners.foreach { pop =>
        val money = currentRecord.moneyOnOwnersPayment / totalPopulation * pop.populationCount
        pop.receiveSalary(money)
      }
    }

    val notSlavesWorkers = currentRecord.peopleResources.filter(_._1.populationType != Slaves)
    val totalEfficiency: Double = notSlavesWorkers.toList.map(efficiency _ tupled).sum

    notSlavesWorkers.foreach { case (pop, count) =>
      val money = currentRecord.moneyOnWorkforceSalary / totalEfficiency * efficiency(pop, count)
      pop.receiveSalary(money)
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
      factoryRecords = factoryRecords.take(FactoryRecordsMax)
    }
  }

  def newDay(taxPolicy: FactoryTaxPolicy): Unit = {
    currentTaxPolicy = taxPolicy
    currentRecord = newFactoryRecord()
  }

  val possibleWorkers: Set[PopulationType]

  def currentMoneyBalance:Double = storage.money

  private def newFactoryRecord() = FactoryRecord(0,0, 0, 0, 0, 0, Map(), Map())

}

case class FulfilledSupplyRequestProfit(request: FulfilledSupplyRequest, profitPerItem: Double)

class IndustrialFactory(product: IndustryProduct, level: Int, startingMoney: Double) extends Factory[IndustryProduct](product, level, startingMoney) {
  override val possibleWorkers: Set[PopulationType] = Set(Craftsmen, Slaves)
}

class MagicFactory(product: MagicProduct, level: Int, startingMoney: Double) extends Factory[MagicProduct](product, level, startingMoney) {
  override val possibleWorkers: Set[PopulationType] = Set(Mages)
}