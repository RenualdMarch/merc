package mr.merc.economics

import MapUtil.FloatOperations._
import Products.{IndustryProduct, Product}
import mr.merc.politics.State

abstract class BusinessProject(productsToBuy: Map[Product, Double]) {

  private var alreadyBought: Map[Product, Double] = Map()
  private var notSpentMoney: Double = 0

  def remainingMoney: Double = notSpentMoney

  def alreadyBoughtProducts: Map[Product, Double] = alreadyBought

  def remainingProducts: Map[Product, Double] = productsToBuy |-| alreadyBought

  def moneyRequired(prices: Map[Product, Double]): Double = {
    (prices |*| remainingProducts).values.sum - notSpentMoney
  }

  def investMoney(money: Double): Unit = {
    notSpentMoney += money
  }

  def takeNotSpentMoney(): Double = {
    val r = notSpentMoney
    notSpentMoney = 0
    r
  }

  def demandRequests(prices: Map[Product, Double]): List[BusinessDemandRequest] = {
    val priceToBuyEverythingNeeded = (remainingProducts |*| prices).values.sum

    val q = if (priceToBuyEverythingNeeded > remainingMoney)
      remainingMoney / priceToBuyEverythingNeeded
    else 1d

    remainingProducts.map { case (p, c) =>
      BusinessDemandRequest(this, p, c * q)
    } toList
  }

  def buyDemandedProducts(requests: List[FulfilledDemandRequest]): Double = {
    val spentMoney = requests.map(_.spentMoney).sum
    notSpentMoney -= spentMoney
    alreadyBought |+|= requests.map { r =>
      r.request.product -> r.bought
    }.toMap
    spentMoney
  }

  def isComplete: Boolean = remainingProducts.values.forall(_ <= 0)

  def takeMoreMoneyFromInvestorIfNeeded(futurePrices: Map[Product, Double]): Unit = {
    val mr = moneyRequired(futurePrices)
    if (mr > 0) {
      takeMoreMoneyFromInvestor(mr)
    }
  }

  def progress:Double = {
    val total = productsToBuy.values.sum
    val gathered = alreadyBoughtProducts.values.sum
    if (total == 0) 1 else gathered / total
  }

  def takeMoreMoneyFromInvestor(neededSum: Double): Double

  def returnNotSpentMoneyToInvestor(): Unit

  def executeProjectAim(): Unit
}

abstract class PopulationBusinessProject(investors: List[Population], cost: Map[Product, Double]) extends BusinessProject(cost) {

  override def takeMoreMoneyFromInvestor(neededSum: Double): Double = {
    val totalMoney = investors.map(_.moneyReserves).sum
    if (totalMoney < neededSum) {
      investors.map(_.investMoney(neededSum)).sum
    } else {
      val proportion = neededSum / totalMoney
      investors.map(i => i.investMoney(i.moneyReserves * proportion)).sum
    }
  }

  override def returnNotSpentMoneyToInvestor(): Unit = {
    val totalEff = investors.map(_.totalPopEfficiency).sum
    if (totalEff == 0) {
      investors.head.receiveSalary(takeNotSpentMoney(), false)
    } else {
      val money = takeNotSpentMoney()
      for (investor <- investors) {
        investor.receiveSalary(money * investor.totalPopEfficiency / totalEff, false)
      }
    }
  }
}


abstract class StateBusinessProject(state: State, cost: Map[Product, Double]) extends BusinessProject(cost) {

  private def budget = state.budget

  override def takeMoreMoneyFromInvestor(neededSum: Double): Double = budget.investMoney(neededSum)

  override def returnNotSpentMoneyToInvestor(): Unit = {
    budget.receiveInvestmentsBack(takeNotSpentMoney())
  }
}

trait ExpandFactoryProject {
  def factory: IndustrialFactory

  def isComplete: Boolean

  def executeProjectAim(): Unit = {
    require(isComplete, "Project not completed!")
    factory.level += 1
  }
}

trait BuildFactoryProject {

  import WorldEconomicConstants.Enterprises._

  def region: EconomicRegion

  def isComplete: Boolean

  def product: IndustryProduct

  def executeProjectAim(): Unit = {
    require(isComplete, "Project not completed!")
    val factory = product match {
      case industry: IndustryProduct =>
        new IndustrialFactory(region, industry, 1, 0,
          FactoryStartingResources, FactoryInputMultiplier, FactoryOutputMultiplier)
    }
    region.enterprises :+= factory
  }
}

class PopulationExpandFactory(val factory: IndustrialFactory, investors: List[Population], cost: Map[Product, Double])
  extends PopulationBusinessProject(investors, cost) with ExpandFactoryProject

class StateExpandFactory(val factory: IndustrialFactory, state: State, cost: Map[Product, Double])
  extends StateBusinessProject(state, cost) with ExpandFactoryProject

class PopulationBuildFactory(val region: EconomicRegion, val product: IndustryProduct,
                             investors: List[Population], cost: Map[Product, Double])
  extends PopulationBusinessProject(investors, cost) with BuildFactoryProject

class StateBuildFactory(val region: EconomicRegion, val product: IndustryProduct,
                        state: State, cost: Map[Product, Double])
  extends StateBusinessProject(state, cost) with BuildFactoryProject
