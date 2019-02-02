package mr.merc.economics

import mr.merc.map.hex.TerrainHexField
import mr.merc.politics.{Province, State}

class WorldState(val regions: List[Province], val playerState:State, val worldHexField: TerrainHexField, var turn: Int = 0) {

  def states:Map[State, List[Province]] = regions.groupBy(_.owner)

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

  def nextTurn(): Unit = {
    turn = turn + 1
    val day = new WorldMarketDay(regions.toSet, turn)
    day.trade()
    regions.foreach { r =>
      val ppd = new PopulationPromotionDemotion(r.regionPopulation)
      ppd.promoteOrDemote()
    }

    // TODO add migrations
  }
}
