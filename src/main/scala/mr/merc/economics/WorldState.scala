package mr.merc.economics

import mr.merc.ai.BattleAI
import mr.merc.army.{Warrior, WarriorCompetence, WarriorType}
import mr.merc.battle.BattleModel
import mr.merc.diplomacy.Claim.{StrongProvinceClaim, VassalizationClaim, WeakProvinceClaim}
import mr.merc.diplomacy.DiplomaticAgreement.{AllianceAgreement, WarAgreement}
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement.TakeMoney
import mr.merc.diplomacy.DiplomaticMessage._
import mr.merc.diplomacy.WorldDiplomacy.RelationshipBonus
import mr.merc.diplomacy._
import mr.merc.economics.Products.IndustryProduct
import mr.merc.economics.TaxPolicy.Income
import mr.merc.economics.WorldConstants.Army.SoldierRecruitmentCost
import mr.merc.economics.WorldGenerationConstants.StateStartingMoney
import mr.merc.economics.WorldStateEnterpriseActions.{FactoryCommand, StateExpandFactoryCommand}
import mr.merc.economics.message.InformationDomesticMessage
import mr.merc.local.Localization
import mr.merc.log.Logging
import mr.merc.players.NamesGenerator
import mr.merc.politics.IssuePosition.{EconomyPosition, RegimePosition}
import mr.merc.politics.Regime.{Absolute, Constitutional, Democracy}
import mr.merc.politics.{Election, Party, Province, State}
import mr.merc.ui.world.ElectionResultsPane
import scalafx.beans.property.ObjectProperty
import mr.merc.util.FxPropertyUtils.PropertyBindingMap
import scalafx.scene.paint.Color

import scala.collection.mutable.ArrayBuffer

class WorldState(val regions: List[Province], val playerState: State, val worldHexField: FourSeasonsTerrainHexField,
                 val namesGenerators:Map[Culture, NamesGenerator], var colorStream:Stream[Color], var turn: Int = 1)
  extends WorldStateParliamentActions
    with WorldStateBudgetActions
    with WorldStateEnterpriseActions
    with WorldStateArmyActions
    with WorldStateDiplomacyActions
    with Logging {

  private val thisTurnBattles = ArrayBuffer[BattleReport]()

  def battleReports:List[BattleReport] = thisTurnBattles.toList

  def playerRegions: List[Province] = regions.filter(_.owner == playerState)

  def states: Map[State, List[Province]] = regions.groupBy(_.owner)

  def initialAiDiplomacy(): Unit = {
    this.aiTurn(onlyAnswer = false)
  }

  def seasonOfYear:SeasonOfYear = Seasons.date(turn)

  def nextTurn(): List[Battle] = {
    thisTurnBattles.clear()
    this.states.keysIterator.foreach(_.mailBox.clearMessages())

    turn = turn + 1
    val day = new WorldMarketDay(this, turn)
    day.trade()
    regions.foreach { r =>
      val ppd = new PopulationMigrationInsideProvince(r.regionPopulation)
      ppd.migrateInsideProvince()
    }

    regions.flatMap { r =>
      val m = new PopulationMigrationOutsideProvince(r)
      m.migrateToNeighbours()
    }.foreach(_.applyMovement())

    this.handlePossibleElections()

    this.processUnansweredMessages()
    this.aiTurn(onlyAnswer = false)
    this.diplomacyEngine.improveBadBoyOverTime()

    info(s"Total money is $totalMoney")

    states.keysIterator.filterNot(_ == playerState).foreach { state =>
      val soldierMovementAI = new SoldierMovementAI(this, state)
      soldierMovementAI.orderSoldiers()
      //soldierMovementAI.moveSoldiers()
    }

    val battlesResolver = new MovementAndBattlesResolver(this)
    val battles = battlesResolver.moveAndPrepareBattles()
    processAiBattles(battles)
    battles
    // TODO add migrations
  }

  def processAiBattles(battles:List[Battle]): Unit = {
    battles.filterNot(_.participants.contains(playerState)).foreach { b =>
      info(s"Battle between ${b.gameField.sides}")
      val model = new BattleModel(b.gameField)
      val aiMap = b.gameField.players.map(p => p -> BattleAI()).toMap
      while(!model.isOver) {
        val event = aiMap(model.currentPlayer).nextTurn(model)
        model.handleEvent(event)
      }
      val report = b.concludeBattle(this)
      info(s"Battle in ${report.provincesInBattle.map(_.name)} with result ${report.result}: side1 is ${report.side1.map(_.name)}, side2 is ${report.side2.map(_.name)}")
      thisTurnBattles += report
    }
  }

  def concludePlayerBattle(battle:Battle): BattleReport = {
    val result = battle.concludeBattle(this)
    thisTurnBattles += result
    result
  }

}

trait WorldStateBudgetActions {
  def regions: List[Province]

  def playerState: State

  def playerRegions: List[Province]

  def totalMoney: Double = totalBudgetMoney + totalPopMoney + totalEnterpriseMoney + totalProjectsMoney +
    totalDemandsMoney + totalSupplyMoney

  def totalBudgetMoney: Double = {
    regions.map(_.owner).distinct.map(_.budget.moneyReserve).sum
  }

  def totalPopMoney: Double = {
    regions.flatMap(_.regionPopulation.pops).map(_.moneyReserves).sum
  }

  def totalEnterpriseMoney: Double = {
    regions.flatMap(_.enterprises).map(_.currentMoneyBalance).sum
  }

  def totalProjectsMoney: Double = {
    regions.flatMap(_.projects).map(_.remainingMoney).sum
  }

  def totalDemandsMoney: Double = regions.flatMap { p =>
    p.regionMarket.fulfilledDemands.flatMap(_._2).map(_.currentSpentMoney)
  }.sum

  def totalSupplyMoney: Double = regions.flatMap { p =>
    p.regionMarket.fulfilledSupply.flatMap(_._2).map(_.currentSpentMoney)
  }.sum

  def setStateTax(state: State, tax: Income, amount: Double): Unit = {
    state.taxPolicy.set(tax, amount)
    state.budget.refreshTaxPolicy()
  }

  def setStateSpending(state: State, spending: SpendingPolicyConfig): Unit = {
    state.budget.spendingPolicyConfig = spending
  }
}

trait WorldStateParliamentActions {

  def playerState: State

  def states: Map[State, List[Province]]

  def turn:Int

  val playerPoliticalSystemProperty: ObjectProperty[PoliticalSystem] =
    ObjectProperty(playerState.politicalSystem)

  private def possiblePartiesRegime: Map[RegimePosition, Set[RegimePosition]] = Map(
    Absolute -> Set(Absolute),
    Constitutional -> Set(Absolute, Constitutional),
    Democracy -> Set(Absolute, Constitutional, Democracy)
  )

  def possibleParties(system: PoliticalSystem): List[Party] = Party.allParties.filter(r =>
    possiblePartiesRegime(system.rulingParty.regime).contains(r.regime))

  def partyPopularityAmongVoters(state: State): Map[Party, Double] = {
    val election = new Election(state.rulingParty, state.primeCulture, possibleParties(state.politicalSystem))
    election.doElections(states(state)).votes
  }

  def partyPopularity(state: State): Map[Party, Double] = {
    val election = new Election(state.rulingParty, state.primeCulture, Party.allParties)
    election.totalPopulationPopularity(states(state)).votes
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
    state.politicalSystem.giveUpPower(newParty, this.turn)
    playerPoliticalSystemProperty.forceInvalidation()
  }

  def possiblePartiesForGivingUpPower(state: State): List[Party] = Party.allParties.filter(
    _.regime == state.rulingParty.regime.freerRegime.get)

  def handlePossibleElections(): Unit = {
    import scalafx.Includes._

    states.keysIterator.foreach { state =>
      if(state.politicalSystem.isElectionNow(turn)) {
        val electionResults = state.politicalSystem.doElectionsNow(turn,
          state.primeCulture, possibleParties(state.politicalSystem), states(state))
        state.mailBox.addMessage(new InformationDomesticMessage(Localization("election.commission"),
          Localization("election.results"), new ElectionResultsPane(electionResults, state)))
      }
    }
  }
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

  def canPlanMoveArmy(from:Province, to:Province, warriorOwner:State) : Boolean

  def planMoveArmy(from:Province, to:Option[Province], warriors:List[Warrior]): Unit = {
    to match {
      case None => from.regionWarriors.planSendWarriors(warriors, None)
      case Some(p) =>
        warriors.groupBy(_.owner).foreach { case (state, list) =>
          if (canPlanMoveArmy(from, p, state))
            from.regionWarriors.planSendWarriors(list, to)
        }
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
  def playerState: State

  def regions: List[Province]

  def states: Map[State, List[Province]]

  def namesGenerators:Map[Culture, NamesGenerator]

  def turn:Int

  var colorStream:Stream[Color]

  val diplomacyEngine = new WorldDiplomacy(this)

  def addClaim(claim:Claim): Unit = {
    diplomacyEngine.addClaim(claim)
  }

  def claims(state:State):List[Claim] = diplomacyEngine.claims(state)

  def notFulfilledClaims(state:State):List[Claim] = claims(state).filter {
    case s:StrongProvinceClaim => s.province.owner != state
    case w:WeakProvinceClaim => w.province.owner != state
    case _:VassalizationClaim => true
  }

  def claimsAgainstStates(state: State):List[State] = notFulfilledClaims(state).map {
    case s:StrongProvinceClaim => s.province.owner
    case w:WeakProvinceClaim => w.province.owner
    case v:VassalizationClaim => v.possibleVassal
  }.distinct

  def generateNewState(culture: Culture, rulingParty:Party):State = {
    val color = colorStream.head
    colorStream = colorStream.tail
    val name = namesGenerators(culture).stateNames.extract()
    new State(name, culture, StateStartingMoney, new PoliticalSystem(rulingParty), color)
  }

  def isPossibleMessage(message: DiplomaticMessage):Boolean = message.isPossible(diplomacyEngine, turn)

  def relationships(state: State): Map[State, Int] = diplomacyEngine.relationships(state, turn)

  def relationshipsDescribed(state: State): Map[State, List[RelationshipBonus]] = diplomacyEngine.relationshipsDescribed(state, turn)

  def sendMessage(message: DiplomaticMessage): Unit = {
    diplomacyEngine.sendMessage(message, turn)

    if (message.from == playerState) {
      aiTurn(onlyAnswer = true)
    }
  }

  def aiTurn(onlyAnswer:Boolean): Unit = {
    val aiStates = states.keySet - playerState
    val ais = aiStates.map(s => new DiplomaticAI(s, this))

    ais.foreach (_.aiMove(onlyAnswer))
    ais.foreach (_.aiMove(onlyAnswer = true))
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

  def canDeclareWar(from:State, to:State): Boolean = {
    val message = new DeclareWar(from, to, new TakeMoney(from, to, 1), Set())
    isPossibleMessage(message)
  }

  def canProposePeace(from:State, to:State): Boolean = {
    diplomacyEngine.wars(from).find(_.sides.contains(to)).exists { wa =>
      val message = ProposePeace(from, to, wa, Set())
      isPossibleMessage(message)
    }
  }

  def warsForWhichCanProposePeace(from:State, to:State):List[WarAgreement] = {
    diplomacyEngine.wars.filter { wa =>
      val leaders = Set(wa.defendersLeader(diplomacyEngine), wa.attackersLeader(diplomacyEngine))
      leaders == Set(from, to)
    }
  }

  def canProposeVassalization(from:State, to:State): Boolean = {
    val message = new VassalizationProposal(from, to)
    isPossibleMessage(message)
  }

  def canProposeOverlordship(from: State, to:State): Boolean = {
    val message = new OverlordshipProposal(from, to)
    isPossibleMessage(message)
  }

  def canProposeAlliance(from: State, to:State): Boolean = {
    val message = new AllianceProposal(from, to)
    isPossibleMessage(message)
  }

  def possibleVassalizationWarTargets(state:State):List[State] = {
    state :: diplomacyEngine.getVassals(state)
  }

  def possibleProvincesToTake(state: State):List[Province] = {
    regions.filter(p => p.owner == state)
  }

  def possibleCulturesToLiberate(state: State):Set[Culture] = {
    regions.filter(_.owner == state).map(_.culture).toSet - state.primeCulture
  }

  def provincesByCulture(state: State, culture: Culture): List[Province] = {
    regions.filter(p => p.owner == state && p.culture == culture)
  }

  def allies(state:State):List[State] = {
    diplomacyEngine.agreements(state).collect {
      case aa:AllianceAgreement => aa.sides - state
    }.flatten
  }

  def vassals(state:State):List[State] = {
    diplomacyEngine.getVassals(state)
  }

  def inWarTogether(state1:State, state2:State):Boolean = {
    diplomacyEngine.wars.exists(ag => ag.onSameSide(Set(state1, state2)))
  }

  def inWarTogetherAgainst(state:State, together:State, against:State):Boolean = {
    diplomacyEngine.wars.exists(ag => ag.onSameSide(Set(state, together)) && ag.onDifferentSides(Set(state, against)))
  }

  def inWarAgainst(state1: State, state2:State):Boolean = {
    diplomacyEngine.wars.exists(ag => ag.onDifferentSides(Set(state1, state2)))
  }

  def mergeAttackersTogether(attackers:Set[State], defender:State):Set[Set[State]] = {
    val defenderWars = diplomacyEngine.wars(defender)
    defenderWars.map { war =>
      val oppositeSide = war.oppositeSideByState(defender)
      attackers & oppositeSide
    }.filter(_.nonEmpty).toSet
  }


  def canPlanMoveArmy(from:Province, to:Province, warriorOwner:State) : Boolean = {
    from.economicNeighbours.contains(to) && canAccessProvince(warriorOwner, to)
  }

  def canAccessProvince(state: State, province:Province): Boolean = {
    state == province.owner || inWarAgainst(state, province.owner) || inWarTogether(state, province.owner) ||
      allies(state).contains(province.owner) || vassals(state).contains(province.owner)
  }
}