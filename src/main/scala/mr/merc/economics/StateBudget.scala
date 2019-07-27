package mr.merc.economics

import mr.merc.economics.TaxPolicy.{Contribution, Income}
import mr.merc.economics.MapUtil.FloatOperations._
import mr.merc.economics.Population._
import mr.merc.economics.SpendingPolicy._

class StateBudget(startingMoney: Double, val taxPolicy: TaxPolicy) {

  private val recordsMax = 30

  private var currentMoney = startingMoney

  def moneyReserve: Double = currentMoney

  var spendingPolicyConfig: SpendingPolicyConfig = SpendingPolicyConfig(0, 0, 0, 0)

  private var currentReport: BudgetDayReport = BudgetDayReport(Map(), Map(), Map())

  private var historyReports: Vector[BudgetDayReport] = Vector()

  def history: Vector[BudgetDayReport] = historyReports

  private var _yesterdaySpendingPolicy: SpendingPolicy = _

  def yesterdaySpendingPolicy: SpendingPolicy = _yesterdaySpendingPolicy

  def dayReport: BudgetDayReport = currentReport

  def endDay(): Unit = {
    historyReports :+= currentReport
    historyReports = historyReports.takeRight(recordsMax)
    currentReport = BudgetDayReport(Map(), Map(), Map())
  }

  def receiveTaxes(taxData: TaxData): Unit = {
    currentReport = currentReport.copy(
      income = currentReport.income |+| Map(taxData.tax -> taxData.taxed),
      grossIncome = currentReport.grossIncome |+| Map(taxData.tax -> taxData.gross))
    currentMoney += taxData.taxed
  }

  def receiveInvestmentsBack(money: Double, spending: Spending): Unit = {
    this.currentMoney += money
    this.currentReport = currentReport.copy(expenses = currentReport.expenses |+| spending -> -money)
  }

  def spendBudgetMoney(regions: List[EconomicRegion], primaryCulture: Culture): Unit = {
    if (regions.isEmpty) {
      _yesterdaySpendingPolicy = SpendingPolicy(0, 0, 0)
      return
    }

    val spendingPolicy = spendingPolicyByConfig(regions)

    val spending = spendingPolicy.scaleSpendingPolicy(moneyReserve)
    _yesterdaySpendingPolicy = spending


    payScholarSalary(regions, primaryCulture, spending.scholarsSalary)
    payBureacracySalary(regions, primaryCulture, spending.bureaucratsSalary)
    payPensions(regions, primaryCulture, spending.pensions)

    currentMoney -= spending.pensions + spending.bureaucratsSalary + spending.bureaucratsSalary
  }

  private def scholarsMoneyPerNeeds(regions: List[EconomicRegion]): Map[PopulationNeedsType, Double] = {
    regions.map { r =>
      r.moneyToFulfillNeeds(Scholars)
    }.reduce(_ |+| _)
  }

  def scholarsSpendingFunction(regions: List[EconomicRegion])(d: Double): Double = {
    scholarsMoneyPerNeeds(regions) dot SpendingPolicyConfig.needsSize(d)
  }

  private def bureaucratsMoneyPerNeeds(regions: List[EconomicRegion]): Map[PopulationNeedsType, Double] = {
    regions.map { r =>
      r.moneyToFulfillNeeds(Bureaucrats)
    }.reduce(_ |+| _)
  }

  def bureaucratsSpendingFunction(regions: List[EconomicRegion])(d: Double): Double = {
    bureaucratsMoneyPerNeeds(regions) dot SpendingPolicyConfig.needsSize(d)
  }

  private def pensionsMoneyPerNeeds(regions: List[EconomicRegion]): Map[PopulationNeedsType, Double] = {
    regions.flatMap { r =>
      r.regionPopulation.pops.filter(_.populationType.populationClass == Lower).map(r.moneyToFulfillNeeds)
    }.reduce(_ |+| _)
  }

  def pensionsSpendingFunction(regions: List[EconomicRegion])(d: Double): Double = {
    pensionsMoneyPerNeeds(regions) dot SpendingPolicyConfig.needsSize(d)
  }

  def armySpendingFunction(regions: List[EconomicRegion])(d: Double): Double = {
    regions.map(_.moneyToFulfillArmyNeeds()).sum * d
  }

  def spendingToPercent(needsMap: Map[PopulationNeedsType, Double])(spending: Double): Double = {
    if (spending <= needsMap(LifeNeeds)) {
      spending / needsMap(LifeNeeds) / 3
    } else if (spending <= needsMap(LifeNeeds) + needsMap(RegularNeeds)) {
      val rem = spending - needsMap(LifeNeeds)
      1d / 3 + rem / needsMap(RegularNeeds) / 3
    } else if (spending <= needsMap(LifeNeeds) + needsMap(RegularNeeds) + needsMap(LuxuryNeeds)) {
      val rem = spending - needsMap(LifeNeeds) - needsMap(RegularNeeds)
      2d / 3 + rem / needsMap(LuxuryNeeds) / 3
    } else 1d
  }

  def scholarsReverseFunction(regions: List[EconomicRegion]): Double => Double = {
    spendingToPercent(scholarsMoneyPerNeeds(regions))
  }

  def bureaucratsReverseFunction(regions: List[EconomicRegion]): Double => Double = {
    spendingToPercent(bureaucratsMoneyPerNeeds(regions))
  }

  def pensionsReverseFunction(regions: List[EconomicRegion]): Double => Double = {
    spendingToPercent(pensionsMoneyPerNeeds(regions))
  }

  def armyReverseFunction(regions: List[EconomicRegion]): Double => Double = {
    d => {
      val max = regions.map(_.moneyToFulfillArmyNeeds()).sum
      if (max != 0) d / max
      else 1d
    }
  }

  def spendingPolicyByConfig(regions: List[EconomicRegion]): SpendingPolicy = {
    SpendingPolicy(scholarsSpendingFunction(regions)(spendingPolicyConfig.scholarsNeeds),
      bureaucratsSpendingFunction(regions)(spendingPolicyConfig.bureaucratsNeeds),
      pensionsSpendingFunction(regions)(spendingPolicyConfig.pensionsNeeds))
  }

  def projectIncomeFunction(income: Income): Double => Double = {
    history.lastOption.map(_.projectionFunction(income)).getOrElse { case _ => 0d }
  }

  private def divideAccordingToEfficiency(regions: List[EconomicRegion], popType: PopulationType,
                                          spending: Spending, money: Double, primaryCulture: Culture): Unit = {
    currentReport = currentReport.copy(expenses = currentReport.expenses |+| Map(spending -> money))
    val pops = regions.flatMap(_.regionPopulation.popsByType(popType))
    val totalEfficiency = pops.map(_.totalPopEfficiency).sum
    if (totalEfficiency != 0) {
      val moneyPerEfficiency = money / totalEfficiency
      pops.foreach { p =>
        p.receiveSalary(p.totalPopEfficiency * moneyPerEfficiency, payTax = false)
      }
    } else {
      assert(money == 0)
    }
  }

  private def payScholarSalary(regions: List[EconomicRegion], primaryCulture: Culture, money: Double): Unit = {
    divideAccordingToEfficiency(regions, Scholars, ScholarsSalary, money, primaryCulture)
  }

  private def payBureacracySalary(regions: List[EconomicRegion], primaryCulture: Culture, money: Double): Unit = {
    divideAccordingToEfficiency(regions, Bureaucrats, BureaucratsSalary, money, primaryCulture)
  }

  private def payPensions(regions: List[EconomicRegion], primaryCulture: Culture, money: Double): Unit = {
    currentReport = currentReport.copy(expenses = currentReport.expenses |+| Pensions -> money)
    val pops = regions.flatMap(_.regionPopulation.pops).filter(p => p.populationType.populationClass == Lower)
    val totalEff = pops.map(_.totalPopEfficiency).sum
    if (totalEff == 0) {
      val moneyPerRegion = money / regions.size
      regions.foreach { r =>
        r.regionPopulation.pop(Farmers, primaryCulture).receiveSalary(moneyPerRegion, payTax = false)
      }
    } else {
      val moneyPerEff = money / totalEff
      pops.foreach { p =>
        p.receiveSalary(moneyPerEff * p.totalPopEfficiency, payTax = false)
      }
    }
  }

  def investMoney(neededMoney: Double, spending: Spending): Double = {
    val investment = if (currentMoney > neededMoney) neededMoney else if (currentMoney > 0) currentMoney else 0
    this.currentMoney -= investment
    this.currentReport = currentReport.copy(expenses = currentReport.expenses |+| spending -> investment)
    investment
  }

  def spendMoneyOnArmySupply(neededMoney: Double): Double = {
    this.currentMoney -= neededMoney
    this.currentReport = currentReport.copy(expenses = currentReport.expenses |+| Army -> neededMoney)
    neededMoney
  }

  def spendMoneyOnReparations(neededMoney: Double): Double = {
    this.currentMoney -= neededMoney
    this.currentReport = currentReport.copy(expenses = currentReport.expenses |+| Reparations -> neededMoney)
    neededMoney
  }

  def receiveMoneyFromReparations(money: Double): Double = {
    this.currentMoney += money
    this.currentReport = currentReport.copy(income = currentReport.income |+| Contribution -> money)
    money
  }
}

case class BudgetDayReport(income: Map[Income, Double],
                           grossIncome: Map[Income, Double],
                           expenses: Map[Spending, Double]) {

  def projectionFunction(income: Income): Double => Double = {
    d => grossIncome.getOrElse(income, 0d) * d
  }
}


case class TaxData(tax: Income, gross: Double, taxed: Double)