package mr.merc.economics

class SupplyDecider {

  private var yesterdayResults: Option[Map[EconomicRegion, FulfilledSupplyRequest]] = None

  def decideSupply(produced: Double, profits: Map[EconomicRegion, EconomicRegionDemand]): Map[EconomicRegion, Double] = {

    if (profits.isEmpty) {
      return Map()
    }

    val actualProfits = profits.filter(_._2.profit > 0).toList.map { case (region, demand) =>
      val actualProfit = yesterdayResults.flatMap(_.get(region)) match {
        case Some(value) =>
          val total = value.request.count
          if (total != 0) {
            demand.profit * value.sold / total
          } else {
            demand.profit
          }

        case None => demand.profit
      }
      (region, demand, actualProfit)
    }

    val total = actualProfits.map { case (_, demand, actualProfit) =>
      actualProfit * demand.count
    }.sum

    if (total == 0) {
      Map()
    } else {
      actualProfits.map { case (region, demand, actualProfit) =>
        region -> (produced * actualProfit * demand.count / total)
      }.toMap
    }
  }

  def receiveSupplyResults(fulfilledSupply: Map[EconomicRegion, FulfilledSupplyRequest]): Unit = {
    yesterdayResults = Some(yesterdayResults.getOrElse(Map()) ++ fulfilledSupply)
  }

  def decideProduction(capacity: Double, unsoldProducts: Double): Double = capacity
}

case class EconomicRegionDemand(count: Double, profit: Double)