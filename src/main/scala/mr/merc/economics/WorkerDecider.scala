package mr.merc.economics

import mr.merc.economics.Population._
import mr.merc.economics.MapUtil.MapWithOperations
import mr.merc.economics.Products.Ritual

class WorkerDecider(region: EconomicRegion, equalPart:Double = 0.5) {
  require(equalPart <= 1 && equalPart >= 0, s"Equal part must be between 0 and 1, but it is $equalPart")

  private val profitPart = 1 - equalPart

  private val workingTypes = Set(Farmers, Labourers, Craftsmen, Mages, Clergy)

  private def workers = region.regionPopulation.pops

  def supplyDemands(demandsMap:Map[Enterprise, Double]):Map[Enterprise, Map[Population, Double]] = {
    val demands = demandsMap.toList.map{case (e, d) => DemandFulfillment(e, d, Map())}

    if (demands.isEmpty) return Map()

    val demandsWithClergy = assignClergy(demands)
    assignFreeWorkers(demandsWithClergy).map(d => d.enterprise -> d.supply).toMap
  }

  private def splitWorkersWithoutOverflows(demands: List[DemandFulfillment], totalEfficiency: Double,
                                           populationType: PopulationType): List[DemandFulfillment] = {
    if (demands.isEmpty) return Nil

    val sortedDemands = demands.sortBy(_.need)
    val avg = totalEfficiency / demands.size

    val head = sortedDemands.head

    if (head.need < avg) {
      val prevNeed = head.need
      head.addSupply(region.regionPopulation.orderPop(populationType, head.need, None)) ::
      splitWorkersWithoutOverflows(sortedDemands.tail, totalEfficiency - prevNeed, populationType)
    } else {
      demands.map(d => d.addSupply(region.regionPopulation.orderPop(populationType, avg, None)))
    }
  }

  private def assignFreeWorkers(demands: List[DemandFulfillment]): List[DemandFulfillment] = {
    (workingTypes - Clergy).foldLeft(demands){ case (list, popType) =>
      val (currentDemands, remained) = list.partition(_.enterprise.possibleWorkers == popType)

      val totalEff = region.regionPopulation.getPopTotalEfficiency(popType)

      val splited = splitWorkersWithoutOverflows(currentDemands, totalEff * equalPart, popType)

      val profitEff = totalEff * profitPart

      val sorted = splited.sortBy(_.enterprise.expectedSalaryPerEfficiency).reverse

      val (result, _) = sorted.foldLeft((List[DemandFulfillment](), profitEff)) { case ((demandsList, remainEff), demand) =>
        import org.scalactic.TripleEquals._
        import org.scalactic.Tolerance._

        if (demand.need === 0.0 +- 0.01) {
          (demand :: demandsList, remainEff)
        } else if (remainEff === 0.0 +- 0.01) {
          (demand :: demandsList, 0)
        } else if (demand.need < remainEff) {
          val fulfilledDemand = demand.addSupply(region.regionPopulation.orderPop(popType, demand.need, None))
          (fulfilledDemand :: demandsList, remainEff - demand.need)
        } else {
          val partlyFulfilledDemand = demand.addSupply(region.regionPopulation.orderPop(popType, remainEff, None))
          (partlyFulfilledDemand :: demandsList, 0)
        }
      }

      result ::: remained
    }
  }

  private def assignClergy(demands: List[DemandFulfillment]): List[DemandFulfillment] = {
    demands.map{d => d.enterprise.product match {
        case Ritual(c) =>
          val pop = region.regionPopulation.orderPop(Clergy, d.need, Some(c))
          d.addSupply(pop)
        case _ => d
      }
    }
  }

  case class DemandFulfillment(enterprise: Enterprise, demand: Double, supply:Map[Population, Double]) {
    def need: Double = demand - supply.map{case (p, c) => p.efficiency * c}.sum
    def addSupply(s:Map[Population, Double]):DemandFulfillment = {
      require(s.values.forall(_ >= 0), s"illegal supply $s")
      val result = this.copy(supply = this.supply |+| s)
      require(result.need >= 0, s"Overflow for need after adding $s to $supply with demand $demand")
      result
    }
  }
}
