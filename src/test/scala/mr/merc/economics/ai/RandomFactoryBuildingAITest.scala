package mr.merc.economics.ai

import mr.merc.economics.Population.{Capitalists, Craftsmen, Humans}
import mr.merc.economics.Culture._
import mr.merc.economics.Products.{Coal, IndustryProduct}
import mr.merc.economics.WorldStateEnterpriseActions.{PopBuildFactoryCommand, PopExpandFactoryCommand}
import mr.merc.economics._
import mr.merc.politics.Economy.StateEconomy
import mr.merc.politics.{Party, PoliticalViews, State}
import org.scalatest.FunSuite

class RandomFactoryBuildingAITest extends FunSuite {

  test("test random capitalists ai") {
    import WorldConstants.Enterprises._
    val ai = new RandomFactoryBuildingAI()
    val state = new State("", LatinHuman, 0,
      Party.absolute.copy(economy = StateEconomy), 0)

    val region = new EconomicRegion {
      override def owner: State = state

      override def economicNeighbours: Set[EconomicRegion] = Set()
      override val regionWarriors: RegionWarriors = null
      override val regionMarket: RegionMarket = new RegionMarket(Map(Coal -> 1))
      override val regionPopulation: RegionPopulation = new RegionPopulation(
        List(
          new Population(LatinHuman, Capitalists, 10, 10000, 0,
            PoliticalViews.averagePoliticalViews),
          new Population(LatinHuman, Craftsmen, EfficiencyPerOneFactoryLevel * 2.5, 0, 0,
            PoliticalViews.averagePoliticalViews))
      )
    }

    val actions = new WorldStateEnterpriseActions {
      override def playerState: State = state

      override def regions: List[EconomicRegion] = List(region)

      override def controlledRegions: List[EconomicRegion] = regions

      override def factoryBuildCost(state: State): Map[Products.Product, Double] = Map(Coal -> 1000)

      override def factoryExpandCost(state: State): Map[Products.Product, Double] = Map(Coal -> 500)
    }

    val commands1 = ai.factoryCommands(region, actions)

    val buildsCommands1 = commands1.collect {
      case x:PopBuildFactoryCommand => x
    }

    val expandCommands1 = commands1.collect {
      case x:PopExpandFactoryCommand => x
    }

    assert(expandCommands1.isEmpty === true)
    assert(buildsCommands1.map(_.product).toSet.size === buildsCommands1.size)
    assert(buildsCommands1.nonEmpty === true)

    Products.AllProducts.collect {
      case p: IndustryProduct => p
    }.foreach { p =>
      region.enterprises +:= new IndustrialFactory(region, p, 1, 0, 0,1, 1)
    }

    val commands2 = ai.factoryCommands(region, actions)

    val buildsCommands2 = commands2.collect {
      case x:PopBuildFactoryCommand => x
    }

    val expandCommands2 = commands2.collect {
      case x:PopExpandFactoryCommand => x
    }

    assert(expandCommands2.isEmpty === false)
    assert(buildsCommands2.isEmpty === true)

    region.factories.values.foreach { f =>
      f.level += 2
    }


    val commands3 = ai.factoryCommands(region, actions)
    assert(commands3.isEmpty === true)

  }
}
