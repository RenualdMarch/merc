package mr.merc.economics

import mr.merc.economics.Population.PopulationType
import mr.merc.economics.Products.Product

trait Enterprise {

  def isResourceGathering: Boolean

  val product: Products.Product

  val region: EconomicRegion

  def produce()

  def sellProduct(demand: Map[EconomicRegion, EconomicRegionDemand]): Map[EconomicRegion, SupplyRequest]

  def receiveSellingResultAndMoney(economicRegion: EconomicRegion, profit:FulfilledSupplyRequestProfit)

  def buyDemandedProducts(requests:List[FulfilledDemandRequest])

  def payMoneyToPops()

  def payTaxes(): Unit

  def endOfDay()

  def expectedSalaryPerEfficiency: Double

  def newDay(taxPolicy: TaxPolicy, bureaucratsPercentage: Double, turn: Int): Unit

  def possibleWorkers: PopulationType

  def owners: List[Population]

  def receiveWorkforceRequest(result: Map[Population, Double])

  def workforceEfficiencyDemand(prices: Map[Product, Double]): Double

  def componentDemandRequests(prices: Map[Product, Double]): Map[Product, DemandRequest]

  def currentMoneyBalance: Double

  def unsoldProducts:Double

  def dayRecords:Vector[DayRecord]

  def reduceStorage()

  protected def paySalaryProportionallyToEfficiency(map:Map[Population, Double], salary: Double): Unit = {
    (map.toList, salary) match {
      case (Nil, sum) =>
        region.regionPopulation.receivePopSalary(possibleWorkers, sum)
      case (_, _) =>
        val totalEfficiency: Double = map.map{case (p, c) => p.efficiency * c}.sum

        map.foreach { case (pop, count) =>
          val money = salary / totalEfficiency * pop.efficiency * count
          pop.receiveSalary(money)
        }
    }
  }
}

trait DayRecord {
  def produced: Double
  def moneyOnWorkforceSalary: Double
  def moneyOnOwnersPayment: Double
  def corporateTax: Double
  def peopleResources:Map[Population, Double]
  def sold: Map[EconomicRegion, FulfilledSupplyRequestProfit]
  def earnings: Double = sold.values.map(p => p.profitPerItem * p.request.sold).sum
  def itemsSold: Double = sold.values.map(_.request.sold).sum
  def turn: Int

  val averageSalary: Double = {
    val resources = peopleResources.values.sum
    if (resources != 0) {
      moneyOnWorkforceSalary / resources
    } else {
      moneyOnWorkforceSalary
    }
  }
}
