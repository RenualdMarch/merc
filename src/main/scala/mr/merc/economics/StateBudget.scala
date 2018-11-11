package mr.merc.economics

import mr.merc.economics.TaxPolicy.Tax
import mr.merc.economics.MapUtil.FloatOperations._

class StateBudget(startingMoney: Double) {

  private var currentMoney = startingMoney

  def moneyReserve: Double = currentMoney

  private var currentReport: BudgetDayReport = _

  def dayReport: BudgetDayReport = currentReport

  def newDay(): Unit = {
    currentReport = BudgetDayReport(Map())
  }

  def receiveTaxes(tax:Tax, money: Double): Unit = {
    currentReport = currentReport.copy(income = currentReport.income |+| Map(tax -> money))
    currentMoney += money
  }

  case class BudgetDayReport(income: Map[Tax, Double])
}

