package mr.merc.economics

class SupplyDecider {

  private val maxSupply = 10

  def decideSupply(produced: Double, profits: Map[EconomicRegion, EconomicRegionDemand]): Map[EconomicRegion, Double] = {
    if (profits.isEmpty) {
      return Map()
    }

    val sortedByProfit = profits.filter(_._2.profit > 0).toList.sortBy(-_._2.profit).take(maxSupply)

    val profitSum = sortedByProfit.map(_._2.profit).sum

    sortedByProfit.map { case (region, demand) =>
      region -> (demand.profit / profitSum * produced)
    }.toMap
  }

  def decideProduction(capacity: Double, inStorage:Double): Double = {
    if (inStorage > capacity) 0 else capacity
  }
}

case class EconomicRegionDemand(count: Double, profit: Double)