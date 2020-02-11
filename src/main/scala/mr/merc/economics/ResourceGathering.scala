package mr.merc.economics

import mr.merc.economics.Population._
import mr.merc.economics.Products._
import mr.merc.economics.ResourceGathering.ResourceGatheringRecord
import mr.merc.economics.TaxPolicy.CorporateTax

abstract class ResourceGathering[Prod <: GatheredProduct](val product: Prod, val region: EconomicRegion, startingProducts: Double, gatheringEfficiencyMultiplier: Double) extends Enterprise {

  private val profitPartToOwners = 0.1
  private val profitPartToWorkers = 0.9

  require(profitPartToOwners + profitPartToWorkers == 1, "Resource gathering is losing money!")

  private val RecordsMax = 30

  private var moneyAccount:Double = 0

  private var unsold = startingProducts

  def isResourceGathering: Boolean = true

  def unsoldProducts:Double = unsold

  private var records: Vector[ResourceGatheringRecord] = Vector()

  private var currentRecord:ResourceGatheringRecord = _

  private var currentTax: Double = 0

  private val supplyDecider = new SupplyDecider()

  def dayRecords:Vector[ResourceGatheringRecord] = records

  private def addCurrentRecord(): Unit = {
    records :+= currentRecord
    if (records.size > RecordsMax) {
      records = records.takeRight(RecordsMax)
    }
  }

  override def newDay(taxPolicy: TaxPolicy, bureaucratsPercentage:Double, turn: Int): Unit = {
    currentTax = taxPolicy.tax(CorporateTax, bureaucratsPercentage)
    currentRecord = newRecord(turn)
  }

  override def produce(): Unit = {
    val efficiency = currentRecord.peopleResources.map{case (p, c) => p.efficiency * c}.sum
    val produced = efficiency * gatheringEfficiencyMultiplier
    unsold = unsold + produced
    currentRecord = currentRecord.copy(produced = produced)
  }

  override def payMoneyToPops(): Unit = {
    val earns = currentRecord.earnings

    currentRecord = currentRecord.copy(
      corporateTax = earns * currentTax,
      moneyOnWorkforceSalary = earns * (1 - currentTax) * profitPartToWorkers,
      moneyOnOwnersPayment = earns * (1 - currentTax) * profitPartToOwners
    )

    moneyAccount -= currentRecord.moneyOnWorkforceSalary
    paySalaryProportionallyToEfficiency(currentRecord.peopleResources, currentRecord.moneyOnWorkforceSalary)
    moneyAccount -= currentRecord.moneyOnOwnersPayment
    paySalaryProportionallyToEfficiency(owners.map(p => p -> p.populationCount.toDouble).toMap, currentRecord.moneyOnOwnersPayment)
  }

  override def payTaxes(): Unit = {
    this.moneyAccount -= currentRecord.corporateTax
    region.owner.budget.receiveTaxes(TaxData(CorporateTax, currentRecord.earnings, currentRecord.corporateTax))
  }

  override def receiveSellingResultAndMoney(region: EconomicRegion, profit:FulfilledSupplyRequestProfit): Unit = {
    currentRecord = currentRecord.copy(sold = currentRecord.sold + (region -> profit))
    unsold = unsold - profit.request.sold
    val receivedMoney = profit.profitPerItem * profit.request.sold
    profit.request.currentSpentMoney -= receivedMoney
    moneyAccount += receivedMoney
  }

  override def receiveWorkforceRequest(result: Map[Population, Double]): Unit = {
    currentRecord = currentRecord.copy(peopleResources = result)
  }

  override def sellProduct(demand: Map[EconomicRegion, EconomicRegionDemand]): Map[EconomicRegion, SupplyRequest] = {
    supplyDecider.decideSupply(unsold, demand).map {case (r, d) =>
      r -> EnterpriseSupplyRequest(this, product, d)
    }
  }

  override def workforceEfficiencyDemand(prices: Map[Products.Product, Double]): Double = {
    val totalEfficiency = region.regionPopulation.getPopTotalEfficiency(possibleWorkers)
    if (unsold > totalEfficiency) 0 else totalEfficiency
  }

  override def endOfDay(): Unit = {
    addCurrentRecord()
  }

  override def owners: List[Population] = region.regionPopulation.pops.filter(_.populationType == Aristocrats)

  override def componentDemandRequests(prices: Map[Products.Product, Double]): Map[Products.Product, DemandRequest] = Map()

  override def expectedSalaryPerEfficiency: Double = {
    records.headOption match {
      case Some(r) =>
        val m = r.moneyOnWorkforceSalary
        val w = r.peopleResources.map {
          case (p, count) => p.efficiency * count
        }.sum
        if (w != 0) m / w
        else m
      case None => 1d
    }
  }

  private def newRecord(turn: Int) = ResourceGatheringRecord(Map(), 0, Map(), 0, 0, 0, 0, turn)

  def buyDemandedProducts(requests:List[FulfilledDemandRequest]): Unit = {
    throw new IllegalStateException("This method shouldn't be called for resource gatherings since they don't have demands")
  }

  def currentMoneyBalance: Double = moneyAccount

}

object ResourceGathering {
  case class ResourceGatheringRecord(peopleResources: Map[Population, Double], produced: Double,
                                     sold: Map[EconomicRegion, FulfilledSupplyRequestProfit],
                                     moneyOnWorkforceSalary: Double, moneyOnOwnersPayment: Double,
                                     unsoldInStorage: Double, corporateTax: Double, turn: Int) extends DayRecord
}

class Farm(product: FarmProduct, region: EconomicRegion, startingProducts: Double, gatheringEfficiencyMultiplier: Double) extends ResourceGathering(product, region, startingProducts, gatheringEfficiencyMultiplier) {
    override def possibleWorkers: Population.PopulationType = Farmers
}

class Mine(product: ResourceProduct, region: EconomicRegion, startingProducts: Double, gatheringEfficiencyMultiplier: Double) extends ResourceGathering(product, region, startingProducts, gatheringEfficiencyMultiplier) {
  override def possibleWorkers: Population.PopulationType = Labourers
}

class Church(product: Ritual, region: EconomicRegion, startingProducts: Double, gatheringEfficiencyMultiplier: Double)
  extends ResourceGathering(product, region, startingProducts, gatheringEfficiencyMultiplier) {
  override def possibleWorkers: Population.PopulationType = Clergy
  override def owners: List[Population] = region.regionPopulation.pops.filter(_.populationType == Clergy)
}

class MagicGuild(region: EconomicRegion, startingMoney: Double,  startingProducts: Double, gatheringEfficiencyMultiplier: Double)
  extends ResourceGathering(Magic, region, startingProducts, gatheringEfficiencyMultiplier) {

  override val possibleWorkers: PopulationType = Mages
  override def owners: List[Population] = region.regionPopulation.pops.filter(_.populationType == Mages)
}