package mr.merc.economics

import org.scalatest.FunSuite

class MarketDayTest extends FunSuite {

  test("oversupply"){
    val marketDay = new MarketDay(Products.Coal, 10, 1)
    val supplyRequest1 = EnterpriseSupplyRequest(null, Products.Coal, 800)
    val supplyRequest2 = EnterpriseSupplyRequest(null, Products.Coal, 200)
    val demandRequest = EnterpriseDemandRequest(null, Products.Coal, 250)
    marketDay.acceptRequests(supplyRequest1, supplyRequest2, demandRequest)
    marketDay.calculateSupplyAndDemand()
    assert(marketDay.fulfilledDemands.get === List(FulfilledDemandRequest(250, 10, demandRequest)))
    assert(marketDay.fulfilledSupply.get.toSet === Set(FulfilledSupplyRequest(200, 10, supplyRequest1),
       FulfilledSupplyRequest(50, 10, supplyRequest2)))
    assert(marketDay.tomorrowPrice.get < 10)
  }

  test("overdemand") {
    val marketDay = new MarketDay(Products.Coal, 10, 1)
    val supplyRequest1 = EnterpriseSupplyRequest(null, Products.Coal, 100)
    val supplyRequest2 = EnterpriseSupplyRequest(null, Products.Coal, 200)
    val demandRequest1 = EnterpriseDemandRequest(null, Products.Coal, 300)
    val demandRequest2 = EnterpriseDemandRequest(null, Products.Coal, 300)
    marketDay.acceptRequests(supplyRequest1, supplyRequest2, demandRequest1, demandRequest2)
    marketDay.calculateSupplyAndDemand()
    assert(marketDay.fulfilledDemands.get.toSet === Set(FulfilledDemandRequest(150, 10, demandRequest1),
       FulfilledDemandRequest(150, 10, demandRequest2)))
    assert(marketDay.fulfilledSupply.get.toSet === Set(FulfilledSupplyRequest(100, 10, supplyRequest1),
       FulfilledSupplyRequest(200, 10, supplyRequest2)))
    assert(marketDay.tomorrowPrice.get > 10)
  }

  test("equal") {
    val marketDay = new MarketDay(Products.Coal, 10, 1)
    val supplyRequest1 = EnterpriseSupplyRequest(null, Products.Coal, 400)
    val supplyRequest2 = EnterpriseSupplyRequest(null, Products.Coal, 200)
    val demandRequest1 = EnterpriseDemandRequest(null, Products.Coal, 500)
    val demandRequest2 = EnterpriseDemandRequest(null, Products.Coal, 100)
    marketDay.acceptRequests(supplyRequest1, supplyRequest2, demandRequest1, demandRequest2)
    marketDay.calculateSupplyAndDemand()
    assert(marketDay.fulfilledDemands.get.toSet === Set(FulfilledDemandRequest(500, 10, demandRequest1),
      FulfilledDemandRequest(100, 10, demandRequest2)))
    assert(marketDay.fulfilledSupply.get.toSet === Set(FulfilledSupplyRequest(400, 10, supplyRequest1),
      FulfilledSupplyRequest(200, 10, supplyRequest2)))
    assert(marketDay.tomorrowPrice.get === 10)
  }

  test("empty market") {
    val marketDay = new MarketDay(Products.Coal, 10, 1)
    marketDay.calculateSupplyAndDemand()
    assert(marketDay.fulfilledDemands.get.isEmpty)
    assert(marketDay.fulfilledSupply.get.isEmpty)
  }

  test("no supply") {
    val marketDay = new MarketDay(Products.Coal, 10, 1)
    val request = EnterpriseDemandRequest(null, Products.Coal, 100)
    marketDay.acceptRequests(request)
    marketDay.calculateSupplyAndDemand()
    assert(marketDay.fulfilledSupply.get.isEmpty)
    assert(marketDay.fulfilledDemands.get === List(FulfilledDemandRequest(0, 10, request)))
  }

  test("no demand") {
    val marketDay = new MarketDay(Products.Coal, 10, 1)
    val request = EnterpriseSupplyRequest(null, Products.Coal, 100)
    marketDay.acceptRequests(request)
    marketDay.calculateSupplyAndDemand()
    assert(marketDay.fulfilledSupply.get === List(FulfilledSupplyRequest(0, 10, request)))
    assert(marketDay.fulfilledDemands.get.isEmpty)
  }

  test("before calculation") {
    val marketDay = new MarketDay(Products.Coal, 10, 1)
    assert(marketDay.tomorrowPrice === None)
    assert(marketDay.fulfilledSupply === None)
    assert(marketDay.fulfilledDemands === None)
  }
}
