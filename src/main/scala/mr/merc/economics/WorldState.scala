package mr.merc.economics

import mr.merc.politics.Province

class WorldState(regions: List[EconomicRegion]) {
  def totalMoney:Double = totalBudgetMoney + totalPopMoney + totalEnterpriseMoney

  def totalBudgetMoney:Double = {
    regions.map(_.owner).distinct.map(_.budget.moneyReserve).sum
  }

  def totalPopMoney: Double = {
    regions.flatMap(_.regionPopulation.pops).map(_.moneyReserves).sum
  }

  def totalEnterpriseMoney: Double = {
    regions.flatMap(_.enterprises).map(_.currentMoneyBalance).sum
  }

}
