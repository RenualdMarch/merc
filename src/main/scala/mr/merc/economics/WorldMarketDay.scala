package mr.merc.economics

import mr.merc.economics.Population.{Clergy, Traders}
import mr.merc.economics.TaxPolicy._
import mr.merc.economics.ai.FactoryBuildingAI
import mr.merc.economics.message.InformationDomesticMessage
import mr.merc.local.Localization
import mr.merc.log.Logging
import mr.merc.map.pathfind.PathFinder
import mr.merc.politics.{Province, State}
import mr.merc.ui.world.BusinessProjectsReportPane
import scalafx.Includes._
import scalafx.scene.layout.Region

class WorldMarketDay(worldState: WorldStateEnterpriseActions with WorldStateDiplomacyActions, turn: Int) extends Logging {
  type PathCache = Map[EconomicRegion, Map[EconomicRegion, List[EconomicRegion]]]

  private val regions: List[EconomicRegion] = worldState.controlledRegions

  private var currentPathCache: PathCache = _

  private def newPathCache(): PathCache = {
    regions.par.map { from =>
      from -> {
        val grid = new EconomicGrid(from, worldState)
        PathFinder.findPossiblePaths(grid, from)
      }
    }.seq.toMap
  }

  private val worldLog = new WorldStateBudgetActions {
    override def regions: List[Province] = worldState.regions.map(_.asInstanceOf[Province])

    override def playerState: State = worldState.playerState

    override def playerRegions: List[Province] = regions.filter(_.owner == playerState)
  }

  def trade(): Unit = {
    currentPathCache = newPathCache()

    val states = regions.map(_.owner).distinct

    // initialize factories and pops
    regions.foreach { r =>
      val b = r.bureaucratsPercentageFromMax
      val taxPolicy = r.owner.taxPolicy
      r.regionPopulation.pops.foreach(_.newDay(taxPolicy, b))
      r.enterprises.foreach(_.newDay(taxPolicy, b, turn))
    }

    // population demands, add them to markets
    // factory demands, add them to markets
    // army demands
    // project demands
    regions.foreach { r =>
      val popDemands = r.regionPopulation.generatePopDemands(r.regionMarket.currentPrices)
      r.regionMarket.acceptDemands(popDemands)
      val enterpriseDemands = r.enterprises.flatMap(_.componentDemandRequests(r.regionMarket.currentPrices).values)
      r.regionMarket.acceptDemands(enterpriseDemands.toList)

      r.regionMarket.acceptDemands(r.regionWarriors.generateArmyNeeds())

      val projectDemands = r.projects.flatMap(_.demandRequests(r.regionMarket.currentPrices))
      r.regionMarket.acceptDemands(projectDemands)
    }

    // let farms, mines and factories send supplies
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
        demands.foreach { demand =>
          demand.request match {
            case p: PopulationDemandRequest =>
              p.pop.buyDemandedProducts(List(demand))
            case e: EnterpriseDemandRequest =>
              e.enterprise.buyDemandedProducts(List(demand))
            case p: BusinessDemandRequest =>
              p.project.buyDemandedProducts(List(demand))
            case w: WarriorDemandRequest =>
              w.warrior.buyDemand(demand)
          }
        }
      }

      r.regionMarket.transferMoneyFromDemandsToSupply()
      // all taxes on supply side
      r.regionMarket.fulfilledSupply.foreach { case (product, supplies) =>
        supplies.foreach { supply =>
          val request = supply.request.asInstanceOf[EnterpriseSupplyRequest]
          val region = request.enterprise.region
          val supplyInfo = buildSupplyInfoForProductAndRegion(product, region, r).get
          val count = supply.sold
          request.enterprise.receiveSellingResultAndMoney(r, FulfilledSupplyRequestProfit(supply, supplyInfo.tradeProfit))
          supplyInfo.price.extractions.foreach(_.payMoney(count, supply))
        }
      }

      r.regionMarket.fulfilledSupply.values.flatten.foreach { fs =>
        require(Math.abs(fs.currentSpentMoney) < 0.01, s"Spent money for supply != 0: $fs")
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

      r.enterprises.foreach(_.reduceStorage())
      r.enterprises.foreach(_.produce())
      r.regionPopulation.pops.foreach(_.fulfillNeedsUsingAlreadyReceivedProducts())
      r.regionWarriors.allWarriors.foreach(_.allNeedsReceived(turn))
    }

    // let all pop receive salaries + taxes to budget and do end of day tasks
    var projectsMap = Map[EconomicRegion, List[BusinessProject]]()
    regions.foreach { r =>
      val factoryCommands = FactoryBuildingAI().factoryCommands(r, worldState)

      r.enterprises.foreach { e =>
        e.payMoneyToPops()
        e.payTaxes()
        e.endOfDay()
      }

      r.regionPopulation.pops.foreach { p =>
        p.payTaxes(r)
        p.endOfDay()
      }

      r.regionMarket.endOfMarketDay(turn)
      val projects = r.removeCompletedProjectsAndAddInvestments()
      projectsMap += (r -> projects)
      factoryCommands.foreach { c => worldState.applyCommand(c) }
      r.removeBankruptFactories()
      r.regionPopulation.learnLiteracy()
      r.regionPopulation.pops.foreach(_.grow())
    }
    val stateProjects = projectsMap.groupBy(_._1.owner)
    states.foreach { s =>
      s.budget.spendBudgetMoney(regions.filter(_.owner == s), s.primeCulture)
      s.budget.endDay()

      val map = stateProjects.getOrElse(s, Map()).asInstanceOf[Map[Province, List[BusinessProject]]]
      s.mailBox.addMessage(new InformationDomesticMessage(s.elites.economyMinister,
        Localization("projectsReport.title")) {
        override def body: Region = new BusinessProjectsReportPane(map)
      })
    }
  }

  private def buildSupplyInfoForProductAndRegion(product: Products.Product, from: EconomicRegion, to: EconomicRegion): Option[SupplyInfo] = {

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

  private case class SupplyInfo(price: Price, path: List[EconomicRegion]) {
    def tradeProfit: Double = price.afterTaxesProfit
  }

}


case class Price(finalPrice: Double, extractions: List[Extraction]) {
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

  def payMoney(count: Double, fulfilledSupplyRequest: FulfilledSupplyRequest): Unit
}

abstract class TradersExtraction(val tradersRegion: EconomicRegion, extractionPart: Double) extends Extraction(extractionPart) {
  def payMoney(count: Double, fulfilledSupplyRequest: FulfilledSupplyRequest): Unit = {
    val money = count * extractionMoney
    fulfilledSupplyRequest.currentSpentMoney -= money
    tradersRegion.regionPopulation.receivePopSalary(Traders, money)
  }
}

class TradersSalesPart(tradersRegion: EconomicRegion) extends TradersExtraction(tradersRegion, 0.1)

class TradersTransitPart(tradersRegion: EconomicRegion) extends TradersExtraction(tradersRegion, 0.05)


class StateTransitPart(val owner: State, region: EconomicRegion) extends Extraction(
  owner.taxPolicy.tax(TransitTax, region.bureaucratsPercentageFromMax)) {
  override def payMoney(count: Double, fulfilledSupplyRequest: FulfilledSupplyRequest): Unit = {
    val money = count * extractionMoney
    fulfilledSupplyRequest.currentSpentMoney -= money
    owner.budget.receiveTaxes(TaxData(TransitTax, count, money))
  }
}

class StateTariffPart(val owner: State, region: EconomicRegion) extends Extraction(
  owner.taxPolicy.tax(TariffTax, region.bureaucratsPercentageFromMax)) {
  override def payMoney(count: Double, fulfilledSupplyRequest: FulfilledSupplyRequest): Unit = {
    val money = count * extractionMoney
    fulfilledSupplyRequest.currentSpentMoney -= money
    owner.budget.receiveTaxes(TaxData(TariffTax, count, money))
  }
}

class StateSalesPart(val owner: State, region: EconomicRegion) extends Extraction(
  owner.taxPolicy.tax(SalesTax, region.bureaucratsPercentageFromMax)) {
  override def payMoney(count: Double, fulfilledSupplyRequest: FulfilledSupplyRequest): Unit = {
    val money = count * extractionMoney
    fulfilledSupplyRequest.currentSpentMoney -= money
    owner.budget.receiveTaxes(TaxData(SalesTax, count, money))
  }
}
