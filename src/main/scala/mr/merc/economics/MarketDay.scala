package mr.merc.economics

import scala.collection.mutable
import Products.Product

class MarketDay(product: Product, val price: Double, val turn: Int) {
  private val priceIncrease = 1.03
  private val priceDecrease = 0.97

  private val lowestPossiblePrice = 0.001

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

  def calculateSupplyAndDemand(): Unit = {
    require(_fulfilledSupply.isEmpty && _fulfilledDemands.isEmpty, "Resulting supply and demand already calculated")

    if (supply.isEmpty && demand.isEmpty) {
      _fulfilledSupply = Some(List())
      _fulfilledDemands = Some(List())
      _tomorrowPrice = Some(price)
      return
    }

    if (totalSupply > totalDemand) {
      _fulfilledDemands = Some(demand.map(d =>  FulfilledDemandRequest(d.count, price, d)).toList)

      val div = totalDemand / totalSupply

      val fulfilledSupplyMap = supply map { s =>
        val sold = s.count * div
        FulfilledSupplyRequest(sold, price, s)
      } toList

      import scala.math.max

      _fulfilledSupply = Some(fulfilledSupplyMap)
      _tomorrowPrice = Some(max(price * priceDecrease, lowestPossiblePrice))
    } else {
      _fulfilledSupply = Some(supply.map(s =>
        FulfilledSupplyRequest(s.count, price, s)).toList)

      val div = totalSupply / totalDemand

      val fulfilledDemandMap = demand map { s  =>
        val bought = s.count * div
        FulfilledDemandRequest(bought, price, s)
      } toList

      _fulfilledDemands = Some(fulfilledDemandMap)

      if (totalSupply == totalDemand) {
        _tomorrowPrice = Some(price)
      } else {
        _tomorrowPrice = Some(price * priceIncrease)
      }

    }
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
case class EnterpriseSupplyRequest(enterprise: Enterprise, override val product: Product, override val count: Double) extends SupplyRequest(product, count)

case class FulfilledDemandRequest(bought: Double, price: Double, request: DemandRequest) {
  def shortage: Double = request.count - bought
}
case class FulfilledSupplyRequest(sold: Double, price: Double, request: SupplyRequest) {
  def excess: Double = request.count - sold
}
