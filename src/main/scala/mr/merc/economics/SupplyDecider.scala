package mr.merc.economics

class SupplyDecider {

  private var yesterdayResults: Option[Map[EconomicRegion, FulfilledSupplyRequest]] = None

  private val positiveCorrection = 1.25
  private val regionsForRemainder = 3

  def decideSupply(produced: Double, profits: Map[EconomicRegion, EconomicRegionDemand]): Map[EconomicRegion, Double] = {
    import scala.math.min
    if (profits.isEmpty) {
      return Map()
    }

    val sortedByProfit = profits.filter(_._2.profit > 0).toList.sortBy(_._2.profit).reverse

    val resultMap = sortedByProfit.foldLeft(Map[EconomicRegion, Double]()) {
      case (map, (region, demand)) =>
        val sum = map.values.sum
        val remained = produced - sum
        if (remained <= 0) map else {
          val yesterdayResult = yesterdayResults.flatMap(_.get(region))
          val supply = yesterdayResult match {
            case None => min(remained, demand.count)
            case Some(result) => if (result.excess > 0) min(result.sold, remained)
            else min(result.sold * positiveCorrection, remained)
          }
          map + (region -> supply)
        }
    }

    if (resultMap.values.sum < produced) {
      val remain = produced - resultMap.values.sum
      val mostProfitableRegions = sortedByProfit.take(regionsForRemainder).map(_._1)
      val supplyList = mostProfitableRegions.map(resultMap)
      val totalSupply = supplyList.sum
      val corrections = supplyList.zip(mostProfitableRegions).map { case (currentSupply, region) =>
        val newSupply = currentSupply + remain * currentSupply / totalSupply
        region -> newSupply
      } toMap

      resultMap ++ corrections
    } else resultMap
  }

  def receiveSupplyResults(fulfilledSupply: Map[EconomicRegion, FulfilledSupplyRequest]): Unit = {
    yesterdayResults = Some(fulfilledSupply)
  }

  def decideProduction(capacity: Double): Double = {
    yesterdayResults match {
      case None => capacity
      case Some(yesterday) =>
        val sold = yesterday.map { case (_, fulfilledRequest) =>
          if (fulfilledRequest.excess > 0) fulfilledRequest.sold
          else fulfilledRequest.sold * positiveCorrection
        }.sum
        scala.math.min(sold, capacity)
    }
  }
}

case class EconomicRegionDemand(count: Double, profit: Double)