package mr.merc.economics

import mr.merc.economics.TaxPolicy.Income
import mr.merc.map.hex.TerrainHexField
import mr.merc.politics.Regime.{Absolute, Democracy}
import mr.merc.politics.{Election, Party, Province, State}
import scalafx.beans.property.ObjectProperty
import mr.merc.util.FxPropertyUtils.PropertyBindingMap

class WorldState(val regions: List[Province], val playerState:State, val worldHexField: TerrainHexField, var turn: Int = 0)
  extends WorldStateParliamentActions
    with WorldStateBudgetActions {

  def playerRegions: List[Province] = regions.filter(_.owner == playerState)

  def states:Map[State, List[Province]] = regions.groupBy(_.owner)


  def nextTurn(): Unit = {
    turn = turn + 1
    val day = new WorldMarketDay(regions.toSet, turn)
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

  def totalMoney:Double = totalBudgetMoney + totalPopMoney + totalEnterpriseMoney

  def totalBudgetMoney:Double = {
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

  def states:Map[State, List[Province]]

  val playerPoliticalSystemProperty:ObjectProperty[PoliticalSystem] =
    ObjectProperty(playerState.politicalSystem)

  def possibleParties(system: PoliticalSystem):List[Party] = Party.allParties.filter(_.regime == system.rulingParty.regime)

  def partyPopularity(state: State):Map[Party, Double] = {
    val election = new Election(state.rulingParty, state.primeCulture, possibleParties(state.politicalSystem))
    election.doElections(states(state)).votes
  }

  val playerCanChangeRulingParty:ObjectProperty[Boolean] = playerPoliticalSystemProperty.map(_.rulingParty.regime == Absolute)

  val playerCanUsurpPower:ObjectProperty[Boolean] = playerPoliticalSystemProperty.map(_.rulingParty.regime != Absolute)

  val playerCanGiveUpPower:ObjectProperty[Boolean] = playerPoliticalSystemProperty.map(_.rulingParty.regime != Democracy)

  def changeRulingParty(state: State, newParty: Party): Unit ={
    state.politicalSystem.changeAbsoluteRulingParty(newParty)
    playerPoliticalSystemProperty.forceInvalidation()
  }

  def usurpPower(state: State, newParty: Party):Unit = {
    state.politicalSystem.usurpPower(newParty)
    playerPoliticalSystemProperty.forceInvalidation()
  }

  def possiblePartiesForUsurpation(state: State):List[Party] = Party.allParties.filter(
    _.regime == state.rulingParty.regime.unfreerRegime.get)

  def giveUpPower(state: State, newParty: Party): Unit ={
    state.politicalSystem.giveUpPower(newParty)
    playerPoliticalSystemProperty.forceInvalidation()
  }

  def possiblePartiesForGivingUpPower(state: State):List[Party] = Party.allParties.filter(
    _.regime == state.rulingParty.regime.freerRegime.get)

}