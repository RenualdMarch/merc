package mr.merc.economics

import mr.merc.economics.Population._
import mr.merc.economics.Products._
import mr.merc.economics.ResourceGathering.ResourceGatheringRecord

abstract class ResourceGathering[Prod <: GatheredProduct](val product: Prod, val region: EconomicRegion, startingProducts: Double, gatheringEfficiencyMultiplier: Double) extends Enterprise {

  private val profitPartToOwners = 0.5
  private val profitPartToWorkers = 0.5

  require(profitPartToOwners + profitPartToWorkers == 1, "Resource gathering is losing money!")

  private val RecordsMax = 30

  private var unsold = startingProducts

  def isResourceGathering: Boolean = true

  def unsoldProducts:Double = unsold

  private var records: Vector[ResourceGatheringRecord] = Vector()

  private var currentRecord:ResourceGatheringRecord = _

  private var currentTax: CorporateTaxPolicy = _

  private val supplyDecider = new SupplyDecider()

  private def addCurrentRecord(): Unit = {
    records :+= currentRecord
    if (records.size > RecordsMax) {
      records = records.takeRight(RecordsMax)
    }
  }

  override def newDay(taxPolicy: CorporateTaxPolicy): Unit = {
    currentTax = taxPolicy
    currentRecord = newRecord()
  }

  override def produce(): Unit = {
    val efficiency = currentRecord.workers.map{case (p, c) => p.efficiency * c}.sum
    val produced = efficiency * gatheringEfficiencyMultiplier
    unsold = unsold + produced
    currentRecord = currentRecord.copy(gathered = produced)
  }

  override def payMoneyToPops(): Unit = {
    val earns = currentRecord.sold.values.map(f => f.profitPerItem * f.request.sold).sum
    val totalEfficiency = currentRecord.workers.map{case (p, c) => p.efficiency * c}.sum

    currentRecord = currentRecord.copy(
      tax = earns * currentTax.corporateTax,
      moneyToWorkers = earns * (1 - currentTax.corporateTax) * profitPartToWorkers,
      moneyToOwners = earns * (1 - currentTax.corporateTax) * profitPartToOwners
    )

    paySalaryProportionallyToEfficiency(currentRecord.workers, currentRecord.moneyToWorkers)
    paySalaryProportionallyToEfficiency(owners.map(p => p -> p.populationCount.toDouble).toMap, currentRecord.moneyToOwners)
  }

  override def payTaxes(): Double = currentRecord.tax


  override def receiveSellingResultAndMoney(region: EconomicRegion, profit:FulfilledSupplyRequestProfit): Unit = {
    currentRecord = currentRecord.copy(sold = currentRecord.sold + (region -> profit))
    unsold = unsold - profit.request.sold
    supplyDecider.receiveSupplyResults(Map(region -> profit.request))
  }

  override def receiveWorkforceRequest(result: Map[Population, Double]): Unit = {
    currentRecord = currentRecord.copy(workers = result)
  }

  override def sellProduct(demand: Map[EconomicRegion, EconomicRegionDemand]): Map[EconomicRegion, SupplyRequest] = {
    supplyDecider.decideSupply(unsold, demand).map {case (r, d) =>
      r -> EnterpriseSupplyRequest(this, product, d)
    }
  }

  override def workforceEfficiencyDemand(prices: Map[Products.Product, Double]): Double = {
    region.regionPopulation.getPopTotalEfficiency(possibleWorkers)
  }

  override def endOfDay(): Unit = {
    addCurrentRecord()
  }

  override def owners: List[Population] = region.regionPopulation.pops.filter(s => s.populationType == Aristocrats || s.populationType == MagicalAristocrats)

  override def componentDemandRequests(prices: Map[Products.Product, Double]): Map[Products.Product, DemandRequest] = Map()

  override def expectedSalaryPerEfficiency: Double = {
    records.headOption match {
      case Some(r) =>
        val m = r.moneyToWorkers
        val w = r.workers.map {
          case (p, count) => p.efficiency * count
        }.sum
        if (w != 0) m / w
        else m
      case None => 1d
    }
  }

  private def newRecord() = ResourceGatheringRecord(Map(), 0, Map(), 0, 0, 0, 0)

  def receiveFulfilledDemandRequestsAndPayChecks(requests:Map[Product, FulfilledDemandRequest]): Unit = {
    throw new IllegalStateException("This method shouldn't be called for resource gatherings since they don't have demands")
  }

  def currentMoneyBalance: Double = 0

}

object ResourceGathering {
  case class ResourceGatheringRecord(workers: Map[Population, Double], gathered: Double, sold: Map[EconomicRegion, FulfilledSupplyRequestProfit],
                                     moneyToWorkers: Double, moneyToOwners: Double, unsoldInStorage: Double, tax: Double)
}

class Farm(product: FarmProduct, region: EconomicRegion, startingProducts: Double, gatheringEfficiencyMultiplier: Double) extends ResourceGathering(product, region, startingProducts, gatheringEfficiencyMultiplier) {
  override def possibleWorkers: Population.PopulationType = Farmers
}

class Mine(product: ResourceProduct, region: EconomicRegion, startingProducts: Double, gatheringEfficiencyMultiplier: Double) extends ResourceGathering(product, region, startingProducts, gatheringEfficiencyMultiplier) {
  override def possibleWorkers: Population.PopulationType = Labourers
}

class Church(product: Ritual, region: EconomicRegion, startingProducts: Double, gatheringEfficiencyMultiplier: Double) extends ResourceGathering(product, region, startingProducts, gatheringEfficiencyMultiplier) {
  override def possibleWorkers: Population.PopulationType = Clergy
  override def owners: List[Population] = region.regionPopulation.pops.filter(_.populationType == Clergy)
}