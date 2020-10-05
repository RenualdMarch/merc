package mr.merc.economics

import scala.collection.mutable
import Products.Product
import mr.merc.army.Warrior
import mr.merc.economics.WorldConstants.Market

class MarketDay(product: Product, val price: Double, val turn: Int) {

  private var _tomorrowPrice:Option[Double] = None

  def tomorrowPrice: Option[Double] = _tomorrowPrice

  private var demand = mutable.ArrayBuffer[DemandRequest]()
  private var supply = mutable.ArrayBuffer[SupplyRequest]()

  private var _fulfilledDemands: Option[List[FulfilledDemandRequest]] = None
  private var _fulfilledSupply: Option[List[FulfilledSupplyRequest]] = None

  def fulfilledDemands: Option[List[FulfilledDemandRequest]] = _fulfilledDemands
  def fulfilledSupply: Option[List[FulfilledSupplyRequest]] = _fulfilledSupply

  def totalSupply: Double = supply.map(_.count).sum
  def totalDemand: Double = demand.map(_.count).sum

  def priorityDemandSize:Double = priorityDemands.map(_.count).sum

  def noPriorityDemandSize:Double = noPriorityDemands.map(_.count).sum

  private def priorityDemands:List[DemandRequest] = {
    demand.collect {
      case w:WarriorDemandRequest => w
    }.toList :::
    demand.collect {
      case bd:BusinessDemandRequest => bd
    }.toList
  }

  private def noPriorityDemands: List[DemandRequest] = {
    demand.collect {
      case _:BusinessDemandRequest => None
      case _:WarriorDemandRequest => None
      case x => Some(x)
    }.flatten.toList
  }

  def calculateSupplyAndDemand(): Unit = {
    require(_fulfilledSupply.isEmpty && _fulfilledDemands.isEmpty, "Resulting supply and demand already calculated")

    if (supply.isEmpty && demand.isEmpty) {
      _fulfilledSupply = Some(List())
      _fulfilledDemands = Some(List())
      _tomorrowPrice = Some(Market.newPrice(price, totalSupply, totalDemand))
      return
    }

    if (totalSupply > totalDemand) {
      _fulfilledDemands = Some(demand.map(d => FulfilledDemandRequest(d.count, price, d)).toList)

      val div = totalDemand / totalSupply

      val fulfilledSupplyMap = supply map { s =>
        val sold = s.count * div
        FulfilledSupplyRequest(sold, price, s)
      } toList

      _fulfilledSupply = Some(fulfilledSupplyMap)
    } else {
      _fulfilledSupply = Some(supply.map(s =>
        FulfilledSupplyRequest(s.count, price, s)).toList)

      val priorityDiv = if (priorityDemandSize != 0) {
        if (totalSupply / priorityDemandSize > 1) 1
        else totalSupply / priorityDemandSize
      } else 0

      val noPriorityDiv = if (totalSupply > priorityDemandSize && noPriorityDemandSize != 0) {
        (totalSupply - priorityDemandSize) / noPriorityDemandSize
      } else 0

      val remainingCount = Math.min(priorityDemandSize, totalSupply)
      val (fulfilledPriorityDemands, _) = priorityDemands.foldLeft((List[FulfilledDemandRequest](), remainingCount)) {
        case ((acc, remaining), s)  =>
        val toFulfill = Math.min(remaining, s.count)
        val newRemaining = remaining - toFulfill
        (FulfilledDemandRequest(toFulfill, price, s) :: acc, newRemaining)
      }

      val fulfilledDemands = fulfilledPriorityDemands ::: noPriorityDemands.map { s  =>
        FulfilledDemandRequest(s.count * noPriorityDiv, price, s)
      }

      _fulfilledDemands = Some(fulfilledDemands)
    }
    _tomorrowPrice = Some(Market.newPrice(price, totalSupply, totalDemand))
  }

  def acceptRequests(r: MarketRequest*): Unit = {
    r.foreach {
      case d:DemandRequest => acceptRequest(d)
      case s:SupplyRequest => acceptRequest(s)
    }
  }

  def acceptRequests(t:TraversableOnce[_ <: MarketRequest]): Unit = {
    t.foreach {
      case d:DemandRequest => acceptRequest(d)
      case s:SupplyRequest => acceptRequest(s)
    }
  }

  private def acceptRequest(r: DemandRequest): Unit = {
    require(r.product == this.product, s"Different products: [${r.product} and [$product]]")
    demand += r
  }

  private def acceptRequest(r: SupplyRequest): Unit = {
    require(r.product == this.product, s"Different products: [${r.product} and [$product]]")
    supply += r
  }

}

sealed abstract class MarketRequest(val product: Product)

sealed abstract class DemandRequest(product: Product, val count: Double) extends MarketRequest(product) {
  require(!count.isNaN, "Count can't be None")
}
sealed abstract class SupplyRequest(product: Product, val count: Double) extends MarketRequest(product) {
  require(!count.isNaN, "Count can't be None")
}
case class PopulationDemandRequest(pop: Population, override val product: Product, override val count: Double) extends DemandRequest(product, count)
case class EnterpriseDemandRequest(enterprise: Enterprise, override val product: Product, override val count: Double) extends DemandRequest(product, count)
case class BusinessDemandRequest(project: BusinessProject, override val product: Product, override val count:Double) extends DemandRequest(product, count)
case class WarriorDemandRequest(warrior: Warrior, override val product: Product, override val count: Double) extends DemandRequest(product, count)
case class EnterpriseSupplyRequest(enterprise: Enterprise, override val product: Product, override val count: Double) extends SupplyRequest(product, count)

case class FulfilledDemandRequest(bought: Double, price: Double, request: DemandRequest, var currentSpentMoney:Double = 0) {
  def shortage: Double = request.count - bought
  def spentMoney:Double = price * bought
  def product: Product = request.product
}
case class FulfilledSupplyRequest(sold: Double, price: Double, request: SupplyRequest, var currentSpentMoney:Double = 0) {
  def excess: Double = request.count - sold
  def receivedMoney:Double = sold * price
  def product: Product = request.product
}
