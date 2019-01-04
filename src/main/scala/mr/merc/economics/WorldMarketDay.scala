package mr.merc.economics

import mr.merc.economics.Population.Traders
import mr.merc.economics.TaxPolicy._
import mr.merc.map.pathfind.PathFinder
import mr.merc.politics.State

class WorldMarketDay(regions: Set[EconomicRegion]) {

  def trade(): Unit = {
    // initialize factories and pops
    regions.foreach { r =>
      r.regionPopulation.pops.foreach(_.newDay(r.owner.taxPolicy.citizensTaxPolicy))
      r.enterprises.foreach(_.newDay(r.owner.taxPolicy.corporateTaxPolicy))
    }

    regions.map(_.owner).foreach(_.budget.newDay())


    // 1 step - receive population demands, add them to markets
    // 2 step - receive factory demands, add them to markets
    regions.foreach { r =>
      val popDemands = r.regionPopulation.generatePopDemands(r.regionMarket.currentPrices)
      r.regionMarket.acceptDemands(popDemands)
      val enterpriseDemands = r.enterprises.map(f => f -> f.componentDemandRequests(r.regionMarket.currentPrices)).toMap
      enterpriseDemands.foreach { case (_, productsMap) =>
        productsMap.foreach { case (p, demand) =>
          r.regionMarket.acceptDemands(List(demand))
        }
      }
    }

    // 3 step - let farms, mines and factories send supplies
    regions.foreach { r =>
      val profitMap = buildSupplyProfitForRegion(r)
      r.enterprises.foreach { e =>
        val map = profitMap(e.product)
        val selling = e.sellProduct(map)
        selling.foreach { case (outputRegion, request) =>
          outputRegion.regionMarket.acceptSupply(Map(e.product -> List(request)))
        }
      }
    }

    // calculate markets, let everyone receive demanded good
    // no taxes for demand
    regions.foreach { r =>
      r.regionMarket.doTrade(Products.AllProducts)
      r.regionMarket.fulfilledDemands.foreach { case (product, demands) =>
        demands.foreach { demand => demand.request match {
            case p:PopulationDemandRequest =>
              p.pop.buyDemandedProducts(List(demand))
            case e:EnterpriseDemandRequest =>
              e.enterprise.receiveFulfilledDemandRequestsAndPayChecks(Map(product -> demand))
          }
        }
      }

      // all taxes on supply side
      r.regionMarket.fulfilledSupply.foreach { case (product, supplies) =>
        supplies.foreach { supply =>
          val request = supply.request.asInstanceOf[EnterpriseSupplyRequest]
          val region = request.enterprise.region
          val supplyInfo = buildSupplyInfoForProductAndRegion(product, region, r).get
          val count = supply.sold
          request.enterprise.receiveSellingResultAndMoney(r, FulfilledSupplyRequestProfit(supply, supplyInfo.tradeProfit))

          supplyInfo.price.extractions.foreach(_.payMoney(count))
        }
      }
    }

    // after all products are received, produce products and calculate pop needs fulfillment
    regions.foreach { r =>
      r.enterprises.foreach(_.produce())
      r.regionPopulation.pops.foreach(_.fulfillNeedsUsingAlreadyReceivedProducts())
    }

    // let all pop receive salaries + taxes to budget
    regions.foreach { r =>
      r.enterprises.foreach{ e =>
        e.payMoneyToPops()
        val money = e.payTaxes()
        r.owner.budget.receiveTaxes(Corporate, money)
        e.endOfDay()
      }

      r.regionPopulation.pops.foreach { p =>
        r.owner.budget.receiveTaxes(Salary, p.payTaxes())
        p.endOfDay()
      }

      r.regionMarket.newMarketDay()
    }
  }

  private def buildSupplyInfoForProductAndRegion(product: Products.Product, from: EconomicRegion, to: EconomicRegion) : Option[SupplyInfo] = {
    val grid = new EconomicGrid(from)

    PathFinder.findOptimalPath(grid, from, to).flatMap { path =>
      val toOwner = to.owner
      val fromOwner = from.owner

      // TODO add economic alliances
      val tariffOpt = if (toOwner == fromOwner) None else Some(new StateTariffPart(toOwner))
      val transitState = path.drop(1).dropRight(1).filter(r => r.owner != from.owner).map { r =>
        new StateTransitPart(r.owner)
      }
      val transitTraders = path.init.map(r => new TradersTransitPart(r))
      val salesTrader = Some(new TradersSalesPart(to))
      val salesState = Some(new StateSalesPart(toOwner))

      val extractions = transitState ++ tariffOpt ++ transitTraders ++ salesTrader ++ salesState

      to.regionMarket.currentPrices.get(product).map { currentPrice =>
        SupplyInfo(Price(currentPrice, extractions), path)
      }
    }
  }

  private def buildSupplyProfitForRegion(region: EconomicRegion): Map[Products.Product, Map[EconomicRegion, EconomicRegionDemand]] = {

    Products.AllProducts.map { p =>
      p -> regions.flatMap { r =>
        buildSupplyInfoForProductAndRegion(p, region, r).map { info =>
          r -> EconomicRegionDemand(region.regionMarket.markets(p).totalDemand,
            info.tradeProfit)
        }
      }.toMap
    }.toMap
  }

  private case class SupplyInfo(price: Price, path:List[EconomicRegion]) {
    def tradeProfit: Double = price.afterTaxesProfit
  }
}


case class Price(finalPrice: Double, extractions:List[Extraction]) {
  val afterTaxesProfit: Double = {
    val taxes = extractions.map(_.extractionPart).sum
    val profit = finalPrice / (1 + taxes)
    extractions.foreach { ex =>
      ex.extractionMoney = profit * ex.extractionPart
    }
    profit
  }
}

sealed abstract class Extraction(val extractionPart: Double) {
  var extractionMoney: Double = 0
  def payMoney(count: Double): Unit
}

abstract class TradersExtraction(val tradersRegion: EconomicRegion, extractionPart: Double) extends Extraction(extractionPart) {
  def payMoney(count: Double): Unit = {
    tradersRegion.regionPopulation.receivePopSalary(Traders, count * extractionMoney)
  }
}

class TradersSalesPart(tradersRegion: EconomicRegion) extends TradersExtraction(tradersRegion, 0)

class TradersTransitPart(tradersRegion: EconomicRegion) extends TradersExtraction(tradersRegion, 0)


class StateTransitPart(val owner: State) extends Extraction(owner.taxPolicy.transitTax.transitTax) {
  override def payMoney(count: Double): Unit = {
    owner.budget.receiveTaxes(Transit, count * extractionMoney)
  }
}

class StateTariffPart(val owner: State) extends Extraction(owner.taxPolicy.tariffTax.tariffTax) {
  override def payMoney(count: Double): Unit = {
    owner.budget.receiveTaxes(Tariff, count * extractionMoney)
  }
}

class StateSalesPart(val owner: State) extends Extraction(owner.taxPolicy.salesTaxPolicy.salesTax) {
  override def payMoney(count: Double): Unit = {
    owner.budget.receiveTaxes(Sales, count * extractionMoney)
  }
}
