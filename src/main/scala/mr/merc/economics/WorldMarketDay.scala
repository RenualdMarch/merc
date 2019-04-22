package mr.merc.economics

import mr.merc.economics.Population.{Clergy, Traders}
import mr.merc.economics.TaxPolicy._
import mr.merc.economics.ai.FactoryBuildingAI
import mr.merc.map.pathfind.PathFinder
import mr.merc.politics.State

class WorldMarketDay(worldState: WorldStateEnterpriseActions, turn:Int) {
  type PathCache = Map[EconomicRegion, Map[EconomicRegion, List[EconomicRegion]]]

  private val regions:List[EconomicRegion] = worldState.regions

  private var currentPathCache: PathCache = _

  private def newPathCache():PathCache = {
    regions.par.map { from =>
      from -> {
        val grid = new EconomicGrid(from)
        PathFinder.findPossiblePaths(grid, from)
      }
    }.seq.toMap
  }

  def trade(): Unit = {
    currentPathCache = newPathCache()

    val states = regions.map(_.owner)

    // initialize factories and pops
    regions.foreach { r =>
      val b = r.bureaucratsPercentageFromMax
      val taxPolicy = r.owner.taxPolicy
      r.regionPopulation.pops.foreach(_.newDay(taxPolicy, b))
      r.enterprises.foreach(_.newDay(taxPolicy, b, turn))
    }

    // 1 step - receive population demands, add them to markets
    // 2 step - receive factory demands, add them to markets
    // 3 step - project demands
    regions.foreach { r =>
      val popDemands = r.regionPopulation.generatePopDemands(r.regionMarket.currentPrices)
      r.regionMarket.acceptDemands(popDemands)
      val enterpriseDemands = r.enterprises.map(f => f -> f.componentDemandRequests(r.regionMarket.currentPrices)).toMap
      enterpriseDemands.foreach { case (_, productsMap) =>
        productsMap.foreach { case (_, demand) =>
          r.regionMarket.acceptDemands(List(demand))
        }
      }
      val projectDemands = r.projects.flatMap(_.demandRequests(r.regionMarket.currentPrices))
      projectDemands.foreach { p =>
        r.regionMarket.acceptDemands(List(p))
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
      r.regionMarket.fulfilledDemands.values.foreach { demands =>
        demands.foreach { demand => demand.request match {
            case p:PopulationDemandRequest =>
              p.pop.buyDemandedProducts(List(demand))
            case e:EnterpriseDemandRequest =>
              e.enterprise.buyDemandedProducts(List(demand))
            case p:BusinessDemandRequest =>
              p.project.buyDemandedProducts(List(demand))
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
      val workforceOrders = r.enterprises.map { e =>
        e -> e.workforceEfficiencyDemand(r.regionMarket.currentPrices)
      }

      workforceOrders.groupBy(_._1.possibleWorkers).foreach { case (pt, orders) =>
        if (pt == Clergy) {
          orders.foreach { case (e, order) =>
            val c = e.asInstanceOf[Church]
            val culture = Some(c.product.culture)
            val workforce = r.regionPopulation.orderPop(pt, order, culture)
            e.receiveWorkforceRequest(workforce)
          }

        } else {
          val workforceDistribution = r.regionPopulation.orderPops(pt, None, orders.toMap)
          workforceDistribution.foreach { case (e, map) =>
            e.receiveWorkforceRequest(map)
          }
        }
      }

      r.enterprises.foreach(_.produce())
      r.regionPopulation.pops.foreach(_.fulfillNeedsUsingAlreadyReceivedProducts())
    }

    // let all pop receive salaries + taxes to budget
    regions.foreach { r =>
      val factoryCommands = FactoryBuildingAI().factoryCommands(r, worldState)

      r.enterprises.foreach{ e =>
        e.payMoneyToPops()
        val money = e.payTaxes()
        r.owner.budget.receiveTaxes(money)
        e.endOfDay()
      }

      r.regionPopulation.pops.foreach { p =>
        val money = p.payTaxes()
        r.owner.budget.receiveTaxes(money)
        p.endOfDay()
      }

      r.regionMarket.endOfMarketDay(turn)
      r.removeCompletedProjectsAndAddInvestments()
      factoryCommands.foreach {c => worldState.applyCommand(c)}
    }

    states.foreach {s =>
      s.budget.spendBudgetMoney(regions.filter(_.owner == s).toList, s.primeCulture)
      s.budget.endDay()
    }
  }

  private def buildSupplyInfoForProductAndRegion(product: Products.Product, from: EconomicRegion, to: EconomicRegion) : Option[SupplyInfo] = {

    currentPathCache(from).get(to).flatMap { path =>
      val toOwner = to.owner
      val fromOwner = from.owner

      // TODO add economic alliances
      val tariffOpt = if (toOwner == fromOwner) None else Some(new StateTariffPart(toOwner, to))
      val transitState = path.drop(1).dropRight(1).filter(r => r.owner != from.owner).map { r =>
        new StateTransitPart(r.owner, r)
      }
      val transitTraders = path.init.map(r => new TradersTransitPart(r))
      val salesTrader = Some(new TradersSalesPart(to))
      val salesState = Some(new StateSalesPart(toOwner, to))

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

class TradersSalesPart(tradersRegion: EconomicRegion) extends TradersExtraction(tradersRegion, 0.1)

class TradersTransitPart(tradersRegion: EconomicRegion) extends TradersExtraction(tradersRegion, 0.05)


class StateTransitPart(val owner: State, region: EconomicRegion) extends Extraction(
  owner.taxPolicy.tax(TransitTax, region.bureaucratsPercentageFromMax)) {
  override def payMoney(count: Double): Unit = {
    owner.budget.receiveTaxes(TaxData(TransitTax, count, count * extractionMoney))
  }
}

class StateTariffPart(val owner: State, region: EconomicRegion) extends Extraction(
  owner.taxPolicy.tax(TariffTax, region.bureaucratsPercentageFromMax)) {
  override def payMoney(count: Double): Unit = {
    owner.budget.receiveTaxes(TaxData(TariffTax, count, count * extractionMoney))
  }
}

class StateSalesPart(val owner: State, region: EconomicRegion) extends Extraction(
  owner.taxPolicy.tax(SalesTax, region.bureaucratsPercentageFromMax)) {
  override def payMoney(count: Double): Unit = {
    owner.budget.receiveTaxes(TaxData(SalesTax, count, count * extractionMoney))
  }
}
