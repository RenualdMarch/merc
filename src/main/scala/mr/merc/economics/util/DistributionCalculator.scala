package mr.merc.economics.util

import mr.merc.economics.MapUtil.FloatOperations._

class DistributionCalculator[T](percentageEqual: Double, percentagePriority: Double, priorityF: T => Double) {
  require(percentageEqual + percentagePriority == 1, "Percentages must be equal to 1")


  def divide(workers: Double, orders: Map[T, Double]):Map[T, Double] = {
    val afterEqual = divideEqualPart(workers * percentageEqual, Map(), orders)
    dividePriorityPart(workers * percentagePriority, afterEqual, orders)
  }

  private def divideEqualPart(workers: Double, alreadyDistributed: Map[T, Double], orders: Map[T, Double]):Map[T, Double] = {
    val toDistribute = orders |-| alreadyDistributed filter(_._2 != 0)
    if (toDistribute.values.sum == 0) {
      alreadyDistributed
    } else {
      val toEveryone = workers / toDistribute.size
      if (toDistribute.values.exists(_ < toEveryone)) {
        val lessThenEveryone = toDistribute.filter(_._2 < toEveryone)
        val remainingWorkers = workers - lessThenEveryone.values.sum
        divideEqualPart(remainingWorkers, alreadyDistributed |+| lessThenEveryone, orders)
      } else {
        val newDistribution = toDistribute.keys.map(_ -> toEveryone).toMap
        newDistribution |+| alreadyDistributed
      }
    }
  }

  private def dividePriorityPart(workers: Double, alreadyDistributed: Map[T, Double], orders: Map[T, Double]): Map[T, Double] = {
    val toDistribute = orders |-| alreadyDistributed filter(_._2 != 0)
    if (workers == 0 || toDistribute.values.sum == 0) {
      alreadyDistributed
    } else {
      val first = toDistribute.keys.toList.maxBy(priorityF)
      val possibleWorkers = math.min(workers, toDistribute(first))
      dividePriorityPart(workers - possibleWorkers, alreadyDistributed |+| Map(first -> possibleWorkers), orders)
    }
  }
}
