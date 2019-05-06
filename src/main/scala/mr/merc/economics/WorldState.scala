package mr.merc.economics

import mr.merc.army.Warrior
import mr.merc.economics.Products.IndustryProduct
import mr.merc.economics.TaxPolicy.Income
import mr.merc.economics.WorldStateEnterpriseActions.{FactoryCommand, PopExpandFactoryCommand, StateExpandFactoryCommand}
import mr.merc.map.hex.TerrainHexField
import mr.merc.politics.IssuePosition.{EconomyPosition, RegimePosition}
import mr.merc.politics.Regime.{Absolute, Constitutional, Democracy}
import mr.merc.politics.{Election, Party, Province, State}
import scalafx.beans.property.ObjectProperty
import mr.merc.util.FxPropertyUtils.PropertyBindingMap

class WorldState(val regions: List[Province], val playerState: State, val worldHexField: TerrainHexField, var turn: Int = 0)
  extends WorldStateParliamentActions
    with WorldStateBudgetActions
    with WorldStateEnterpriseActions {

  def playerRegions: List[Province] = regions.filter(_.owner == playerState)

  def states: Map[State, List[Province]] = regions.groupBy(_.owner)

  def nextTurn(): Unit = {
    turn = turn + 1
    val day = new WorldMarketDay(this, turn)
    day.trade()
    regions.foreach { r =>
      val ppd = new PopulationPromotionDemotion(r.regionPopulation)
      ppd.promoteOrDemote()
    }

    // TODO add migrations
  }


}

trait WorldStateBudgetActions {
  def regions: List[Province]

  def playerState: State

  def playerRegions: List[Province]

  def totalMoney: Double = totalBudgetMoney + totalPopMoney + totalEnterpriseMoney

  def totalBudgetMoney: Double = {
    regions.map(_.owner).distinct.map(_.budget.moneyReserve).sum
  }

  def totalPopMoney: Double = {
    regions.flatMap(_.regionPopulation.pops).map(_.moneyReserves).sum
  }

  def totalEnterpriseMoney: Double = {
    regions.flatMap(_.enterprises).map(_.currentMoneyBalance).sum
  }

  def setStateTax(state: State, tax: Income, amount: Double): Unit = {
    state.taxPolicy.set(tax, amount)
  }

  def setStateSpending(state: State, spending: SpendingPolicyConfig): Unit = {
    state.budget.spendingPolicyConfig = spending
  }
}

trait WorldStateParliamentActions {

  def playerState: State

  def states: Map[State, List[Province]]

  val playerPoliticalSystemProperty: ObjectProperty[PoliticalSystem] =
    ObjectProperty(playerState.politicalSystem)

  private def possiblePartiesRegime: Map[RegimePosition, Set[RegimePosition]] = Map(
    Absolute -> Set(Absolute),
    Constitutional -> Set(Absolute, Constitutional),
    Democracy -> Set(Absolute, Constitutional, Democracy)
  )

  def possibleParties(system: PoliticalSystem): List[Party] = Party.allParties.filter(r =>
    possiblePartiesRegime(system.rulingParty.regime).contains(r.regime))

  def partyPopularity(state: State): Map[Party, Double] = {
    val election = new Election(state.rulingParty, state.primeCulture, Party.allParties)
    election.doElections(states(state)).votes
  }

  val playerCanChangeRulingParty: ObjectProperty[Boolean] = playerPoliticalSystemProperty.map(_.rulingParty.regime == Absolute)

  val playerCanUsurpPower: ObjectProperty[Boolean] = playerPoliticalSystemProperty.map(_.rulingParty.regime != Absolute)

  val playerCanGiveUpPower: ObjectProperty[Boolean] = playerPoliticalSystemProperty.map(_.rulingParty.regime != Democracy)

  def changeRulingParty(state: State, newParty: Party): Unit = {
    state.politicalSystem.changeAbsoluteRulingParty(newParty)
    playerPoliticalSystemProperty.forceInvalidation()
  }

  def usurpPower(state: State, newParty: Party): Unit = {
    state.politicalSystem.usurpPower(newParty)
    playerPoliticalSystemProperty.forceInvalidation()
  }

  def possiblePartiesForUsurpation(state: State): List[Party] = Party.allParties.filter(
    _.regime == state.rulingParty.regime.unfreerRegime.get)

  def giveUpPower(state: State, newParty: Party): Unit = {
    state.politicalSystem.giveUpPower(newParty)
    playerPoliticalSystemProperty.forceInvalidation()
  }

  def possiblePartiesForGivingUpPower(state: State): List[Party] = Party.allParties.filter(
    _.regime == state.rulingParty.regime.freerRegime.get)

}

object WorldStateEnterpriseActions {

  trait FactoryCommand {
    def buildBusinessProject(actions: WorldStateEnterpriseActions): BusinessProject

    def state:State

    def economyPolicy:EconomyPosition = state.rulingParty.economy

    def isValid:Boolean

    def region:EconomicRegion
  }

  case class StateExpandFactoryCommand(state: State, factory: IndustrialFactory) extends FactoryCommand {
    override def buildBusinessProject(actions: WorldStateEnterpriseActions) = new StateExpandFactory(factory, state, actions.factoryExpandCost(state))

    override def isValid: Boolean = economyPolicy.stateCanExpandFactory

    def region:EconomicRegion = factory.region
  }

  case class StateBuildFactoryCommand(state: State, product: IndustryProduct, region: EconomicRegion) extends FactoryCommand {
    override def buildBusinessProject(actions: WorldStateEnterpriseActions) = new StateBuildFactory(region, product, state, actions.factoryBuildCost(state))

    override def isValid: Boolean = economyPolicy.stateCanBuildFactory &&
      !region.presentFactoriesAndProjects.contains(product)
  }

  case class PopExpandFactoryCommand(state: State, investors: List[Population], factory: IndustrialFactory) extends FactoryCommand {
    override def buildBusinessProject(actions: WorldStateEnterpriseActions) = new PopulationExpandFactory(factory, investors, actions.factoryExpandCost(state))

    override def isValid: Boolean = economyPolicy.capitalistsCanExpandFactory

    def region:EconomicRegion = factory.region
  }

  case class PopBuildFactoryCommand(state: State, investors: List[Population], product: IndustryProduct, region: EconomicRegion) extends FactoryCommand {
    override def buildBusinessProject(actions: WorldStateEnterpriseActions) = new PopulationBuildFactory(region, product, investors, actions.factoryBuildCost(state))

    override def isValid: Boolean = economyPolicy.capitalistsCanBuildFactory &&
      !region.presentFactoriesAndProjects.contains(product)
  }

}

trait WorldStateEnterpriseActions {
  def playerState: State

  def regions: List[EconomicRegion]

  import WorldConstants.Enterprises._
  import MapUtil.FloatOperations._

  def stateCanExpandFactory(factory:IndustrialFactory):Boolean = {
    StateExpandFactoryCommand(factory.region.owner, factory).isValid
  }

  def stateCanBuildFactory(state: State):Boolean = {
    state.rulingParty.economy.stateCanBuildFactory
  }

  def factoryExpandCost(state:State):Map[Products.Product, Double] = {
    FactoryExpandCost |*| state.rulingParty.economy.investmentCostMultiplier
  }

  def factoryBuildCost(state:State):Map[Products.Product, Double] = {
    FactoryBuildCost |*| state.rulingParty.economy.investmentCostMultiplier
  }

  def applyCommand(command:FactoryCommand): Unit = {
    if (command.isValid) {
      val project = command.buildBusinessProject(this)
      command.region.projects ::= project
    }
  }
}

trait WorldStateArmyMovementActions {
  def playerState: State

  def regions: List[Province]

  def canPlanMoveArmy(from:Province, to:Province) : Boolean = {
    from.economicNeighbours.contains(to) && from.owner == to.owner
  }

  def planMoveArmy(from:Province, to:Option[Province], warriors:List[Warrior]): Unit = {
    to match {
      case None => from.regionWarriors.planSendWarriors(warriors, None)
      case Some(p) => if (canPlanMoveArmy(from, p)) from.regionWarriors.planSendWarriors(warriors, to)
    }
  }

}