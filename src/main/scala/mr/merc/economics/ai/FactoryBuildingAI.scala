package mr.merc.economics.ai

import mr.merc.economics.Population.{Aristocrats, Capitalists, Craftsmen}
import mr.merc.economics._
import mr.merc.economics.WorldConstants.Enterprises._
import mr.merc.economics.MapUtil.FloatOperations._
import mr.merc.economics.Products.IndustryProduct
import mr.merc.economics.WorldStateEnterpriseActions.{FactoryCommand, PopBuildFactoryCommand, PopExpandFactoryCommand}
import mr.merc.util.WeightedRandom

object FactoryBuildingAI {

  def apply():FactoryBuildingAI = new RandomFactoryBuildingAI()
}

trait FactoryBuildingAI {

  def factoryCommands(currentRegion: EconomicRegion, state: WorldStateEnterpriseActions):List[FactoryCommand]
}

class RandomFactoryBuildingAI extends FactoryBuildingAI {

  override def factoryCommands(currentRegion: EconomicRegion, state: WorldStateEnterpriseActions):List[FactoryCommand] = {
    val prices = currentRegion.regionMarket.currentPrices
    val factoryBuildCost = state.factoryBuildCost(currentRegion.owner) dot prices
    val factoryExpandCost = state.factoryExpandCost(currentRegion.owner) dot prices
    val maxTask = Math.max(factoryBuildCost, factoryExpandCost)

    val capInvestors = currentRegion.regionPopulation.popsByType(Capitalists)
    val capMoneyForNeeds = currentRegion.moneyToFulfillNeeds(Capitalists).values.sum
    val capTotalMoney = Math.max(0, capInvestors.map(_.moneyReserves).sum - capMoneyForNeeds)
    val (investors, totalMoney) = if (capTotalMoney / maxTask.toInt == 0) {
      val investors = capInvestors //++ currentRegion.regionPopulation.popsByType(Aristocrats)
      val moneyForNeeds = capMoneyForNeeds //+ currentRegion.moneyToFulfillNeeds(Aristocrats).values.sum
      val totalMoney = Math.max(0, investors.map(_.moneyReserves).sum - moneyForNeeds)
      (investors, totalMoney)
    } else (capInvestors, capTotalMoney)

    val projects = currentRegion.projects.collect {
      case p:PopulationBusinessProject => p
    }.size
    val productsSize = Math.max(0, (totalMoney / maxTask).toInt - projects)
    val random = new WeightedRandom(Products.IndustryProducts.map(p => p -> 1d).toMap)
    val productsToChange = random.nextRandomItems(productsSize).groupBy(identity).mapValues(_.size) -- noNeedToAddMore(currentRegion)
    val presentProducts = currentRegion.presentFactoriesAndProjects
    val absentProducts = productsToChange.keySet -- presentProducts
    val productsToExpand = productsToChange -- absentProducts
    absentProducts.toList.map { p =>
      PopBuildFactoryCommand(currentRegion.owner, investors, p, currentRegion)
    } ::: productsToExpand.flatMap { case (p, count) =>
      (0 until count).flatMap {_ =>
        currentRegion.factories.get(p).map { f =>
          PopExpandFactoryCommand(currentRegion.owner, investors, f)
        }
      }
    }.toList
  }

  private def noNeedToAddMore(region:EconomicRegion): Set[IndustryProduct] = {
    val totalRegionEfficiency = region.regionPopulation.popsByType(Craftsmen).map(_.totalPopEfficiency).sum
    val maxLevels = totalRegionEfficiency / EfficiencyPerOneFactoryLevel
    region.factories.filter { case (p, f) => f.level >= maxLevels}.keySet
  }

}
