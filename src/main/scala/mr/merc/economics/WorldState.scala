package mr.merc.economics

import mr.merc.army.{Warrior, WarriorType}
import mr.merc.army.WarriorType.WarriorCompetence
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement
import mr.merc.diplomacy.DiplomaticMessage.DeclareWar
import mr.merc.diplomacy.WorldDiplomacy.RelationshipBonus
import mr.merc.diplomacy._
import mr.merc.economics.Products.IndustryProduct
import mr.merc.economics.TaxPolicy.Income
import mr.merc.economics.WorldConstants.Army.SoldierRecruitmentCost
import mr.merc.economics.WorldGenerationConstants.StateStartingMoney
import mr.merc.economics.WorldStateEnterpriseActions.{FactoryCommand, PopExpandFactoryCommand, StateExpandFactoryCommand}
import mr.merc.map.hex.TerrainHexField
import mr.merc.players.NamesGenerator
import mr.merc.politics.IssuePosition.{EconomyPosition, RegimePosition}
import mr.merc.politics.Regime.{Absolute, Constitutional, Democracy}
import mr.merc.politics.{Election, Party, Province, State}
import scalafx.beans.property.ObjectProperty
import mr.merc.util.FxPropertyUtils.PropertyBindingMap
import scalafx.scene.paint.Color

class WorldState(val regions: List[Province], val playerState: State, val worldHexField: TerrainHexField,
                 val namesGenerators:Map[Culture, NamesGenerator], var colorStream:Stream[Color], var turn: Int = 0)
  extends WorldStateParliamentActions
    with WorldStateBudgetActions
    with WorldStateEnterpriseActions
    with WorldStateArmyActions
    with WorldStateDiplomacyActions {

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

trait WorldStateArmyActions {
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

  def recruitSoldier(province: Province, competence: WarriorCompetence, warriorType: WarriorType, warriorCulture: Culture): Unit = {
    val project = new StateRecruitWarrior(province, province.owner, SoldierRecruitmentCost(competence), warriorType, competence, warriorCulture)
    province.projects ::= project
  }

  def disposeSoldier(p: Province, w: Warrior): Unit = {
    p.regionWarriors.takeWarriors(List(w))
  }

  def possibleWarriorsToRecruit(p: Province): List[(WarriorType, WarriorCompetence, Culture)] = {
    val ownerCulture = p.owner.primeCulture
    ownerCulture.warriorViewNames.possibleWarriors.map { case ((wt, wc), _) =>
      (wt, wc, ownerCulture)
    }.toList
  }
}

trait WorldStateDiplomacyActions {
  //def playerState: State

  def regions: List[Province]

  def namesGenerators:Map[Culture, NamesGenerator]

  def turn:Int

  var colorStream:Stream[Color]

  private val diplomacyEngine = new WorldDiplomacy(this)

  def addClaim(claim:Claim): Unit = {
    diplomacyEngine.addClaim(claim)
  }

  def generateNewState(culture: Culture, rulingParty:Party):State = {
    val color = colorStream.head
    colorStream = colorStream.tail
    val name = namesGenerators(culture).stateNames.extract()
    new State(name, culture, StateStartingMoney, new PoliticalSystem(rulingParty), color)
  }

  def possibleMessages(from:State, to:State):List[DiplomaticMessage] = ???

  def relationships(state: State): Map[State, Int] = diplomacyEngine.relationships(state, turn)

  def relationshipsDescribed(state: State): Map[State, List[RelationshipBonus]] = diplomacyEngine.relationshipsDescribed(state, turn)

  def sendMessage(message: DiplomaticMessage): Unit = {
    diplomacyEngine.sendMessage(message, turn)
  }

  def mailbox(state:State):List[DiplomaticMessage] = {
    diplomacyEngine.messages(state, turn)
  }

  def acknowledgeMessage(declaration:DiplomaticDeclaration): Unit = {
    diplomacyEngine.answerMessage(declaration, turn)
  }

  def answerMessage(proposal:DiplomaticProposal, answerIsYes:Boolean): Unit = {
    diplomacyEngine.answerMessage(proposal, answerIsYes, turn)
  }

  def defaultCustomMessageAnswer(custom:CustomDiplomaticQuestion): Unit = {
    diplomacyEngine.defaultAnswerMessage(custom, turn)
  }

  def answerDeclareWar(dw:DeclareWar, allies:Set[State]): Unit = {
    diplomacyEngine.answerDeclareWarMessage(dw, turn, allies)
  }

  def processUnansweredMessages(): Unit = {
    diplomacyEngine.processAllUnansweredMessages(turn)
  }

  def agreements(state: State):List[DiplomaticAgreement] = diplomacyEngine.agreements(state)


  def attackerLeader(warAgreement: WarAgreement):State = warAgreement.attackersLeader(diplomacyEngine)

  def defendersLeader(warAgreement: WarAgreement):State = warAgreement.defendersLeader(diplomacyEngine)
}