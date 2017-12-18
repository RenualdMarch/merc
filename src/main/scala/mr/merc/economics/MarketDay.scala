package mr.merc.economics

import scala.collection.mutable

class MarketDay(price: Double) {
  private val priceIncrease = 1.03
  private val priceDecrease = 0.97

  private val lowestPossiblePrice = 0.001

  private var _tomorrowPrice:Option[Double] = None

  def tomorrowPrice: Option[Double] = _tomorrowPrice

  private var demand = mutable.ArrayBuffer[DemandRequest]()
  private var supply = mutable.ArrayBuffer[SupplyRequest]()

  private var _fulfilledDemands: Option[Map[DemandRequest, FulfilledDemandRequest]] = None
  private var _fulfilledSupply: Option[Map[SupplyRequest, FulfilledSupplyRequest]] = None

  def fulfilledDemands: Option[Map[DemandRequest, FulfilledDemandRequest]] = _fulfilledDemands
  def fulfilledSupply: Option[Map[SupplyRequest, FulfilledSupplyRequest]] = _fulfilledSupply

  def calculateSupplyAndDemand(): Unit = {
    require(_fulfilledSupply.isEmpty && _fulfilledDemands.isEmpty, "Resulting supply and demand already calculated")

    if (supply.isEmpty && demand.isEmpty) {
      _fulfilledSupply = Some(Map())
      _fulfilledDemands = Some(Map())
      _tomorrowPrice = Some(price)
      return
    }

    val totalSupply = supply.foldLeft(0d)(_ + _.count)
    val totalDemand = demand.foldLeft(0d)(_ + _.count)

    if (totalSupply > totalDemand) {
      _fulfilledDemands = Some(demand.map(d =>
        d -> FulfilledDemandRequest(d.count, 0, price)).toMap)

      val div = totalDemand / totalSupply

      val fulfilledSupplyMap = supply map { s =>
        val sold = s.count * div
        s -> FulfilledSupplyRequest(sold, s.count - sold, price)
      } toMap

      import scala.math.max

      _fulfilledSupply = Some(fulfilledSupplyMap)
      _tomorrowPrice = Some(max(price * priceDecrease, lowestPossiblePrice))
    } else {
      _fulfilledSupply = Some(supply.map(s =>
        s -> FulfilledSupplyRequest(s.count, 0, price)).toMap)

      val div = totalSupply / totalDemand

      val fulfilledDemandMap = demand map { s  =>
        val bought = s.count * div
        s -> FulfilledDemandRequest(bought, s.count - bought, price)
      } toMap

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
    demand += r
  }

  private def acceptRequest(r: SupplyRequest): Unit = {
    supply += r
  }

}

// don't make it case classes, it will break using it as a key in result map
sealed abstract class MarketRequest
final class DemandRequest(val count: Double) extends MarketRequest
final class SupplyRequest(val count: Double) extends MarketRequest


case class FulfilledDemandRequest(bought: Double, shortage: Double, price: Double)
case class FulfilledSupplyRequest(sold: Double, excess: Double, price: Double)
