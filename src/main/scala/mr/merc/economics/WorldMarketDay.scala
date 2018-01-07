package mr.merc.economics

class WorldMarketDay(regions: Set[EconomicRegion]) {


  def trade(): Unit = {

    // 1 step - receive population demands, add them to markets
    regions.foreach { r =>
      val demands = r.pops.generatePopDemands(r.regionMarket.currentPrices)
      r.regionMarket.acceptDemands(demands)
    }

    // 2 step - receive factory demands, add them to markets

    // third step - let farms and mines send supplies

    // calculate markets, let factory receive demanded good and produce products

    // factories send supplies

    // calculate markets again, and two steps with factories sending demands then supplies

    // after all products are finished, calculate pop needs fulfillment

    // let all pop receive salaries + taxes to budget
    regions.foreach { r =>
      val demands = r.pops.generatePopDemands(r.regionMarket.currentPrices)

    }

    Products.ProductTradingRounds
  }
}
