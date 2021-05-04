package mr.merc.politics

import mr.merc.economics.SeasonOfYear
import mr.merc.local.Localization
import mr.merc.politics.HeadOfState.RulingTime
import mr.merc.politics.Regime.Democracy
import mr.merc.util.MercUtils._

class Elites(state: State, turn: Int, startingRulingParty: Party) {

  private var currentRulingParty: Party = startingRulingParty

  private var prevRulers: List[HeadOfState.RulingTime] = Nil

  private var currentRulerStart: Int = turn

  private var currentRuler = {
    if (startingRulingParty.regime == Regime.Democracy)
      newPresident(turn)
    else newMonarch(turn)
  }

  def stateRuler: HeadOfState = currentRuler

  private var currentForeignMinister = newOfficial(Localization("foreign.minister"), turn)

  def foreignMinister: Official = currentForeignMinister

  private var currentDefenceMinister = newOfficial(Localization("defence.minister"), turn)

  def defenceMinister: Official = currentDefenceMinister

  private var currentEconomyMinister = newOfficial(Localization("economic.minister"), turn)

  def economyMinister: Official = currentEconomyMinister

  def refreshElites(turn: Int): Unit = {
    reloadMinistersIfNeeded(turn)
    reloadHeadOfStateIfNeeded(turn)
    currentRulingParty = state.rulingParty
  }

  private def reloadMinisterIfNeeded(turn: Int, official: Official): Official = {
    if (!official.isAlive(turn) || state.rulingParty != currentRulingParty) {
      newOfficial(official.jobTitle, turn)
    } else official
  }

  private def reloadMinistersIfNeeded(turn: Int): Unit = {
    currentForeignMinister = reloadMinisterIfNeeded(turn, currentForeignMinister)
    currentEconomyMinister= reloadMinisterIfNeeded(turn, currentEconomyMinister)
    currentDefenceMinister = reloadMinisterIfNeeded(turn, currentDefenceMinister)
  }

  private def reloadHeadOfStateIfNeeded(turn: Int): Unit = {
    if (!currentRuler.isAlive(turn)) {
      replaceRuler(turn)
    } else if (state.rulingParty.regime == Democracy &&
      currentRulingParty != state.rulingParty) {
      replaceRuler(turn)
    }
  }

  def presidentUsurpsPower(turn: Int): Unit = {
    replaceRuler(turn)
    currentRuler = currentRuler match {
      case monarch: Monarch => monarch
      case president: President => president.toMonarch(monarchNumber(president.name))
    }
    currentRulingParty = state.rulingParty
  }

  private def replaceRuler(turn: Int): Unit = {
    val rulingTime = RulingTime(SeasonOfYear.date(currentRulerStart), SeasonOfYear.date(turn - 1), currentRuler)
    prevRulers ::= rulingTime
    currentRulerStart = turn
    currentRuler = newRuler(turn)
  }

  private def newRuler(turn: Int): HeadOfState = {
    if (state.rulingParty.regime == Regime.Democracy)
      newPresident(turn)
    else newMonarch(turn)
  }

  private def monarchNumber(name: String): Int = prevRulers.map(_.head).collect {
    case m: Monarch if m.name == name => m
  }.size + 1

  private def newMonarch(turn: Int): Monarch = {
    val culture = state.primeCulture
    val (born, death) = Person.generateBirthAndDeath(turn, culture.race)
    val (wt, wc) = culture.warriorViewNames.possibleRulers.keys.randomElement()
    val name = culture.randomMaleName
    new Monarch(name, monarchNumber(name), state, wt, wc, born, death)
  }

  private def newPresident(turn: Int): President = {
    val culture = state.primeCulture
    val (born, death) = Person.generateBirthAndDeath(turn, culture.race)
    val (wt, wc) = culture.warriorViewNames.possibleRulers.keys.randomElement()

    new President(culture.randomMaleName, state, wt, wc, born, death)
  }

  private def newOfficial(jobTitle: String, turn: Int): Official = {
    val culture = state.primeCulture
    val (born, death) = Person.generateBirthAndDeath(turn, culture.race)
    val (wt, wc) = culture.warriorViewNames.possibleNobles.keys.randomElement()
    new Official(culture.randomMaleName, jobTitle, state, culture, wt, wc, born, death)
  }
}
