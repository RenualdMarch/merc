package mr.merc.economics

import mr.merc.map.PossibleGrid
import Products.{IndustryProduct, Product}
import mr.merc.economics.Population._
import mr.merc.economics.util.DistributionCalculator
import mr.merc.politics.{PoliticalViews, Province, State}
import mr.merc.economics.MapUtil.FloatOperations.MapWithFloatOperations
import WorldConstants.Population._
import cats.Monoid
import mr.merc.army.Warrior
import mr.merc.economics.EconomicRegion.{ProductTradingInfo, ProductionTradingInfo}
import mr.merc.economics.RegionPopulation.Rebellion
import mr.merc.log.Logging
import EconomicRegion._
import mr.merc.util.RecalculatableValue

import scala.util.Random

trait EconomicRegion {

  def owner:State

  def economicNeighbours:Set[EconomicRegion]

  val regionMarket:RegionMarket

  val regionPopulation: RegionPopulation

  val regionWarriors: RegionWarriors

  var enterprises:Vector[Enterprise] = Vector()

  def factories:Map[IndustryProduct, IndustrialFactory] = enterprises.collect {
    case f:IndustrialFactory => f.product -> f
  }.toMap

  def presentFactoriesAndProjects:Set[IndustryProduct] = {
    factories.keySet ++ projects.collect {
      case p:BuildFactoryProject => p.product
    }
  }

  def moneyToFulfillNeeds(populationType: PopulationType): Map[PopulationNeedsType, Double] = {
    val pops = regionPopulation.popsByType(populationType)
    val currentMoney = pops.map(_.moneyReserves).sum
    val map = regionPopulation.popsByType(populationType).map(moneyToFulfillNeeds).
      foldLeft(Map[PopulationNeedsType, Double]())(_ |+| _).withDefaultValue(0d)

    if (currentMoney <= map(LifeNeeds)) {
      val rem = map(LifeNeeds) - currentMoney
      map + (LifeNeeds -> rem)
    } else if (currentMoney <= map(LifeNeeds) + map(RegularNeeds)) {
      val rem = map(LifeNeeds) + map(RegularNeeds) - currentMoney
      map + (LifeNeeds -> 0d) + (RegularNeeds -> rem)
    } else if (currentMoney <= map.values.sum) {
      val rem = map.values.sum - currentMoney
      Map(LifeNeeds -> 0d, RegularNeeds -> 0d, LuxuryNeeds -> rem)
    } else {
      Map(LifeNeeds -> 0d, RegularNeeds -> 0d, LuxuryNeeds -> 0d)
    }
  }

  def moneyToFulfillNeeds(population: Population): Map[PopulationNeedsType, Double] = {
    population.needs.transform { case (_, needs) =>
      needs dot regionMarket.currentPrices
    }
  }

  def moneyToFulfillArmyNeeds(): Double = {
    regionWarriors.allWarriors.map(_.needs).reduceOption(_ |+| _).map(_ dot regionMarket.currentPrices).getOrElse(0)
  }

  var projects:List[BusinessProject] = Nil

  def removeCompletedProjectsAndAddInvestments(): List[BusinessProject] = {
    val allProjects = projects
    val (finished, notFinished) = projects.partition(_.isComplete)
    this.projects = notFinished
    finished.foreach { p =>
      p.returnNotSpentMoneyToInvestor()
      p.executeProjectAim()
    }
    notFinished.foreach { p =>
      p.takeMoreMoneyFromInvestorIfNeeded(regionMarket.currentPrices)
    }
    allProjects
  }

  def removeBankruptFactories(): Unit = {
    enterprises.collect {
      case e:IndustrialFactory if e.isBankrupt => e
    }.foreach { f =>
      if (f.level > 1) {
        f.decreaseLevel()
      } else {
        f.giveBudgetToOwners()
      }
    }

    enterprises = enterprises.collect {
      case f:IndustrialFactory if !f.isBankrupt => f
      case x => x
    }
  }

  private val bureaucratsPercentageFromMaxVariable = new RecalculatableValue[Double](bureaucratsPercentageFromMaxF)

  def bureaucratsPercentageFromMax:Double = bureaucratsPercentageFromMaxVariable.value

  private def bureaucratsPercentageFromMaxF:Double = {
    val totalPopulation = regionPopulation.popsList.map(_.populationCount).sum
    val bureaucrats = regionPopulation.popsByType(Bureaucrats).map(_.populationCount).sum
    if (totalPopulation == 0) 0
    else {
      val div = bureaucrats.toDouble / totalPopulation
      val max = totalPopulation * WorldConstants.Population.BureaucratsPercentageForMaxEff
      div / max
    }
  }

  def goodsProducedLastTurn:Map[Product, Double] = {
    enterprises.map { e =>
      e.dayRecords.lastOption.map { day =>
        Map(e.product -> day.produced)
      }.getOrElse(Map())
    }.reduceOption(_ |+| _).getOrElse(Map())
  }

  def gdp:Double = {
    goodsProducedLastTurn dot regionMarket.currentPrices
  }

  def soldToMarket:Map[State, ProductionTradingInfo] = {
    enterprises.flatMap { e =>
      e.dayRecords.lastOption.map { day =>
        day.sold.map { case (region, profit) =>
          region -> ProductTradingInfo(e.product, profit.request.sold, profit.request.receivedMoney)
        }
      }.getOrElse(Map()).toList
    }.groupBy(_._1.owner).map { case (owner, list) =>
      owner -> Monoid.combineAll(list.map { case (_, pti) =>
        ProductionTradingInfo(Map(pti.product -> pti))
      })
    }
  }

  def civilianVictimsOfBattleDied(): Unit = {
    regionPopulation.nonEmptyPops.foreach(_.kill(WorldConstants.Population.BattleVictimsPercentage))
  }

  def endTurn(): Unit = {
    bureaucratsPercentageFromMaxVariable.clear()
  }
}

class EconomicGrid(region:EconomicRegion, diplomacy: WorldStateDiplomacyActions) extends PossibleGrid[EconomicRegion] {

  override def isBlocked(t: EconomicRegion): Boolean = if (region.owner != t.owner) {
    !diplomacy.canTrade(region.owner, t.owner)
  } else false

  def neighbours(t: EconomicRegion): List[EconomicRegion] = t.economicNeighbours.toList

  // currently we think that movement from one province to another takes same time
  override def price(from: EconomicRegion, to: EconomicRegion): Double = {
    val stateTransit = if (region.owner == from.owner) 0 else new StateTransitPart(from.owner, from).extractionPart
    1 + new TradersTransitPart(from).extractionPart + stateTransit
  }

  override def cellWhereItIsForbiddenToStop(t: EconomicRegion): Boolean = false

  override def cellWhereMovementMustBeStopped(t: EconomicRegion): Boolean = false
}

object RegionPopulation {

  case class Rebellion(province: Province, pops:List[Population]) {

    require(pops.nonEmpty, "Rebeliion pops cann't be empty")
    val rebellionCulture:Culture = pops.groupBy(_.culture).map{case (cul, list) => cul -> list.map(_.populationCount).sum}.maxBy(_._2)._1

    val totalRebels:Int = pops.map(_.populationCount).sum
    require(totalRebels > 0, s"Total rebels count cann't be $totalRebels")
  }
}

class RegionPopulation(initialPops: List[Population]) {

  import mr.merc.util.MercUtils._

  private var currentPops = initialPops.groupBy(_.populationType)

  def generatePopDemands(prices:Map[Product, Double]): List[PopulationDemandRequest] = {
    popsList.flatMap { pop =>
      pop.calculateDemands(prices).map { case (p, c) =>
        PopulationDemandRequest(pop, p, c)
      }
    }
  }

  def receivePopSalary(popType: PopulationType, money: Double): Unit = {
    val current = popsByType(popType)
    val total = current.map(p => p.populationCount * p.efficiency).sum
    total match {
      case 0 => current.head.receiveSalary(money)
      case s => current.foreach { p =>
        p.receiveSalary(money * p.populationCount * p.efficiency / s)
      }
    }
  }

  def popsByType(populationType: PopulationType): List[Population] = {
    pops.getOrElse(populationType, Nil)
  }

  def popsByTypeAndCulture(populationType: PopulationType, culture: Option[Culture]):List[Population] = {
    popsByType(populationType).filter(p => culture.forall(_ == p.culture))
  }

  def grow(): Unit = {
    popsList.foreach(_.grow())
  }

  def orderPop(populationType: PopulationType, requiredEfficiency: Double, culture: Option[Culture]): Map[Population, Double] = {
    val populations = popsByTypeAndCulture(populationType, culture)
    val totalEfficiency = populations.map(_.totalPopEfficiency).sum
    if (totalEfficiency == 0) {
      Map()
    } else {
      val part = requiredEfficiency / totalEfficiency
      if (part >= 1) {
        populations.map(p => p -> p.populationCount.toDouble).toMap
      } else if (part == 0) {
        Map()
      } else {
        populations.map(p => p -> p.populationCount * part).toMap
      }
    }
  }

  def orderPops(populationType: PopulationType, culture: Option[Culture], orders: Map[Enterprise, Double]): Map[Enterprise, Map[Population, Double]] = {
    val dc = new DistributionCalculator[Enterprise](0.1, 0.9,
      e => e.dayRecords.lastOption.map(_.moneyOnWorkforceSalary).getOrElse(0d))

    val totalEfficiency = popsByTypeAndCulture(populationType, culture).map(_.totalPopEfficiency).sum

    dc.divide(totalEfficiency, orders).transform { case (_, eff) =>
      orderPop(populationType, eff, culture)
    }
  }

  def pop(p: PopulationType, c: Culture): Population = {
    popsByType(p).find(_.culture == c) match {
      case Some(x) => x
      case None =>
        val views = PoliticalViews.initPoliticalViews(p)
        val pop = new Population(c, p, 0, 0, 0, views)
        currentPops = currentPops.addToList(p, pop)
        pop
    }
  }

  def consumptionHappiness(populationType: PopulationType): Double = {
    val pops = popsByType(populationType)
    if (pops.nonEmpty) {
      val sum = pops.map(_.populationCount).sum
      pops.map(p => p.consumptionHappiness * p.populationCount).sum / sum
    } else EmptyPopConsumptionHappiness
  }

  def getPopTotalEfficiency(populationType: PopulationType): Double =
    popsByType(populationType).map(_.totalPopEfficiency).sum

  def pops:Map[PopulationType, List[Population]] = currentPops

  def popsList:List[Population] = pops.values.flatten.toList

  def nonEmptyPops:List[Population] = popsList.filter(_.populationCount > 0)

  def cultureMembers:Map[Culture, Int] = popsList.groupBy(_.culture).mapValues(_.map(_.populationCount).sum)

  def learnLiteracy(): Unit = {
    val scholars = popsByType(Scholars).map(_.populationCount).sum.toDouble
    val literacyIncrease = scholars * ScholarsLiteracyLearningIncreaseMultiplier
    val totalCount = popsList.map(_.populationCount).sum.toDouble
    popsList.foreach { p =>
      val perc = literacyIncrease * p.populationCount / totalCount
      p.learnLiteracy(perc)
    }
  }

  def populationCount: Int = popsList.map(_.populationCount).sum

  private def popsWhoWantToRebel(state: State, random: Random = Random): List[Population] = {
    import WorldConstants.Population._
    popsList.filter(_.salary.size > 2).filter { p =>
      val chance = popRebellingChance(p.consumptionHappiness, p.politicalHappiness(state))
      random.nextDouble() < chance
    }
  }

  private def partWantsToRebel(rebelPops:List[Population]):Double = {
    val totalPops = popsList.map(_.populationCount).sum
    val rebels = rebelPops.map(_.populationCount).sum
    if (totalPops != 0) rebels.toDouble / totalPops
    else 0d
  }

  private def rebellionTakesPlace(rebelPops:List[Population]): Boolean = {
    val parts = partWantsToRebel(rebelPops)
    parts > WorldConstants.Population.RebellionPopulationPart
  }

  def rebellion(province: Province, random: Random = Random):Option[Rebellion] = {
    val rebels = popsWhoWantToRebel(province.controller, random)
    if (rebellionTakesPlace(rebels)) {
      Some(Rebellion(province, rebels))
    } else None
  }
}

class RegionMarket(initialPrices:Map[Product, Double]) extends Logging {
  private var historicalData = initialPrices.keys.map { p =>
    p -> Vector[MarketDay]()
  }.toMap

  private val maxRecords = 30

  def history:Map[Products.Product, Vector[MarketDay]] = historicalData

  private var marketDaysForProduct = initialPrices.map { case (product, price) =>
    product -> new MarketDay(product, price, 1)
  }

  def markets: Map[Product, MarketDay] = marketDaysForProduct

  def acceptDemands(demands:List[DemandRequest]): Unit = {
    demands.foreach { dr =>
      marketDaysForProduct(dr.product).acceptRequests(dr)
    }
  }

  def acceptSupply(supply: Map[Product, List[SupplyRequest]]): Unit = {
    supply.foreach { case (p, list) =>
      marketDaysForProduct(p).acceptRequests(list)
    }
  }

  def fulfilledDemands:Map[Product, List[FulfilledDemandRequest]] = {
    marketDaysForProduct.map { case (p, market) =>
      p -> market.fulfilledDemands.getOrElse(Nil)
    }
  }

  def fulfilledSupply: Map[Product, List[FulfilledSupplyRequest]] = {
    marketDaysForProduct.map { case (p, market) =>
      p -> market.fulfilledSupply.getOrElse(Nil)
    }
  }

  def doTrade(products:List[Product]): Unit = {
    products.map(marketDaysForProduct).foreach(_.calculateSupplyAndDemand())
  }

  def transferMoneyFromDemandsToSupply(): Unit = {
    val totalMoneyOnBuying = fulfilledDemands.values.flatten.map(_.currentSpentMoney).sum
    val totalMoneyOnSelling = fulfilledSupply.values.flatten.map(_.receivedMoney).sum

    val consistencyCheck = Math.abs(totalMoneyOnBuying - totalMoneyOnSelling) < 0.01

    if (!consistencyCheck) {
      warn("Fulfilled demands: " + fulfilledDemands.toString())
      warn("Fulfilled supply: " +  fulfilledSupply.toString())
    }

    require(consistencyCheck,
      s"Buying and selling differ, buying is $totalMoneyOnBuying, selling is $totalMoneyOnSelling")

    fulfilledDemands.values.flatten.foreach(_.currentSpentMoney = 0)
    fulfilledSupply.values.flatten.foreach (s => s.currentSpentMoney = s.receivedMoney)
  }

  def endOfMarketDay(turn:Int): Unit = {
    marketDaysForProduct.foreach { case (p, md) =>
      historicalData += p -> (historicalData(p) :+ md).takeRight(maxRecords)
    }

    marketDaysForProduct = marketDaysForProduct.map { case (p, m) =>
      p -> new MarketDay(p, m.tomorrowPrice.getOrElse(sys.error(s"Market for $p was not closed!")), turn + 1)
    }

    recalculateCurrentPrices()
  }

  private var _currentPricesCached: Map[Product, Double] = initialPrices

  private def recalculateCurrentPrices(): Unit = {
    _currentPricesCached = marketDaysForProduct.map { case (product, market) =>
      product -> market.price
    }
  }

  def currentPrices: Map[Product, Double] = _currentPricesCached
}

class RegionWarriors(initial: List[Warrior], neighbours: => Set[EconomicRegion]) {
  private var destinations:Map[Option[EconomicRegion], List[Warrior]] = Map(None -> initial)

  def allWarriors:List[Warrior] = warriorDestinations.values.flatten.toList

  def warriorDestinations:Map[Option[EconomicRegion], List[Warrior]] = destinations

  def planSendWarriors(whom:List[Warrior], to:Option[EconomicRegion]): Unit = {
    require(to.forall(neighbours.contains), s"Illegal destination $to, possible $neighbours")
    require(whom.toSet.subsetOf(allWarriors.toSet), s"planned send warriors $whom but present only $allWarriors")

    val withoutWhom = destinations.transform { case (_, list) =>
        list.filterNot(whom.contains)
    }
    val newTo = withoutWhom.getOrElse(to, Nil) ::: whom
    destinations = withoutWhom + (to -> newTo)
  }

  def sendWarriorsToDestinations():Map[EconomicRegion, List[Warrior]] = {
    val remain = destinations.getOrElse(None, Nil)
    val result = destinations.collect {
      case (Some(region), list) => region -> list
    }
    destinations = Map(None -> remain)
    result
  }

  def receiveWarriors(list:List[Warrior]): Unit = {
    val present = destinations.getOrElse(None, Nil)
    destinations += None -> (present ::: list)
  }

  def takeWarriors(list:List[Warrior]): List[Warrior] = {

    val result = list.filter(allWarriors.contains)

    destinations = destinations.transform { case (_, l) =>
      l.filterNot(list.contains)
    }

    result
  }

  def clearDeadWarriors(): List[Warrior] = {
    val dead = allWarriors.filterNot(_.isAlive)

    takeWarriors(dead)
  }

  def generateArmyNeeds(): List[WarriorDemandRequest]= {
    allWarriors.flatMap { w =>
      val budgetArmyNeeds = w.owner.budget.spendingPolicyConfig.armyNeeds
      if (w.owner.budget.moneyReserve > 0 && budgetArmyNeeds > 0) {
        (w.needs |*| budgetArmyNeeds).map { case (p, c) =>
          WarriorDemandRequest(w, p, c)
        }
      } else Nil
    }
  }
}

object EconomicRegion {
  implicit val ProductionTradingInfoMonoid:Monoid[ProductionTradingInfo] = new Monoid[ProductionTradingInfo] {
    override def empty: ProductionTradingInfo = ProductionTradingInfo(Map())

    override def combine(x: ProductionTradingInfo, y: ProductionTradingInfo): ProductionTradingInfo = {
      val intersection = x.perProduct.keySet & y.perProduct.keySet
      val xOnly = x.perProduct.keySet -- intersection
      val xOnlyMap = x.perProduct.filterKeys(xOnly.contains)
      val yOnly = y.perProduct.keySet -- intersection
      val yOnlyMap = y.perProduct.filterKeys(yOnly.contains)
      val intersectionMap = intersection.map { k =>
        val xp = x.perProduct(k)
        val yp = y.perProduct(k)
        k -> ProductTradingInfo(k, xp.count + yp.count, xp.totalPrice + yp.totalPrice)
      }.toMap

      ProductionTradingInfo(xOnlyMap ++ yOnlyMap ++ intersectionMap)
    }
  }

  case class ProductionTradingInfo(perProduct:Map[Product, ProductTradingInfo]) {
    lazy val totalSum: Double = perProduct.values.map(_.totalPrice).sum
  }
  case class ProductTradingInfo(product: Product, count: Double, totalPrice: Double)
}