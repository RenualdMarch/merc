package mr.merc.economics.ai

import mr.merc.economics.Population.{Capitalists, Craftsmen}
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
    val investors = currentRegion.regionPopulation.popsByType(Capitalists)
    val totalMoney = investors.map(_.moneyReserves).sum
    val prices = currentRegion.regionMarket.currentPrices
    val factoryBuildCost = state.factoryBuildCost(currentRegion.owner) dot prices
    val factoryExpandCost = state.factoryExpandCost(currentRegion.owner) dot prices
    val maxTask = Math.max(factoryBuildCost, factoryExpandCost)
    val productsSize = (totalMoney / maxTask).toInt
    val random = new WeightedRandom(Products.IndustryProducts.map(p => p -> 1d).toMap)
    val productsToChange = random.nextRandomItems(productsSize).groupBy(identity).mapValues(_.size) -- noNeedToAddMore(currentRegion)
    val presentProducts = currentRegion.presentFactoriesAndProjects
    val absentProducts = productsToChange.keySet -- presentProducts
    val productsToExpand = productsToChange -- absentProducts
    absentProducts.toList.map { p =>
      PopBuildFactoryCommand(currentRegion.owner, investors, p, currentRegion)
    } ::: productsToExpand.flatMap { case (p, count) =>
      (0 until count).map {_ =>
        PopExpandFactoryCommand(currentRegion.owner, investors, currentRegion.factories(p))
      }
    }.toList
  }

  private def noNeedToAddMore(region:EconomicRegion): Set[IndustryProduct] = {
    val totalRegionEfficiency = region.regionPopulation.popsByType(Craftsmen).map(_.totalPopEfficiency).sum
    val maxLevels = totalRegionEfficiency / EfficiencyPerOneFactoryLevel
    region.factories.filter { case (p, f) => f.level >= maxLevels}.keySet
  }

}
