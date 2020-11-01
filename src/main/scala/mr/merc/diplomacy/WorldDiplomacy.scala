package mr.merc.diplomacy

import mr.merc.diplomacy.DiplomaticAgreement.{AllianceAgreement, TruceAgreement, VassalAgreement, WarAgreement}
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement._
import mr.merc.diplomacy.Claim.{ProvinceClaim, StrongProvinceClaim, VassalizationClaim, WeakProvinceClaim}
import mr.merc.diplomacy.DiplomaticMessage.DeclareWar
import mr.merc.diplomacy.WorldDiplomacy.RelationshipBonus
import mr.merc.politics.{Province, State}
import mr.merc.economics.WorldConstants.Diplomacy._
import mr.merc.economics.{Culture, WorldConstants, WorldStateDiplomacyActions}
import mr.merc.local.Localization

import scala.util.Random


class WorldDiplomacy(actions: WorldStateDiplomacyActions) {
  def regions: List[Province] = actions.regions

  private var currentTurnMessages:List[DiplomaticMessage] = Nil

  def turnMessagesReport:List[DiplomaticMessage] = currentTurnMessages.reverse

  private var savedTurn = -1

  private def states: Set[State] = regions.toSet.map { p: Province => p.owner }

  private var agreements: List[DiplomaticAgreement] = Nil
  private var events: List[RelationshipEvent] = Nil

  private var _badBoy: Map[State, Double] = Map()

  def badBoy: Map[State, Double] = _badBoy

  private var mailbox: Map[State, List[DiplomaticMessage]] = Map()

  private var claims: Set[Claim] = Set()

  def allClaims: Set[Claim] = claims

  def generateInitialStrongClaimsForOwnedTerritories(): Unit = {
    require(claims.isEmpty, s"Claims is not empty: $claims")
    claims = regions.map(p => StrongProvinceClaim(p.owner, p)).toSet
  }

  def generateInitialClaimsForNeighbours(): Unit = {
    generateClaimsForNeighbours(WorldConstants.Diplomacy.ChanceForInitialWeakClaim,
      WorldConstants.Diplomacy.WeakClaimTime)
  }

  def generateEndTurnClaimsForNeighbours(currentTurn:Int): Unit = {
    generateClaimsForNeighbours(WorldConstants.Diplomacy.ChanceForWeakClaim,
      currentTurn + WorldConstants.Diplomacy.WeakClaimTime)
  }

  private def generateClaimsForNeighbours(percentage: Double, claimEnd:Int): Unit = {
    for {
      p <- regions
      neig <- p.neighbours if !claims.contains(StrongProvinceClaim(p.owner, neig))
    } {
      if (Random.nextDouble() < percentage) {
        claims += WeakProvinceClaim(p.owner, neig, claimEnd)
      }
    }
  }

  def replaceWeakClaimsWithStrongClaimsForOwnedTerritories(currentTurn:Int): Unit = {
    claims.collect {
      case wc: WeakProvinceClaim => wc
    }.filter (wc => wc.province.owner == wc.state).foreach { wc =>
      if (wc.claimTurnEnd == currentTurn + 1) {
        claims = claims + StrongProvinceClaim(wc.state, wc.province) - wc
      }
    }
  }

  def hasClaimOverProvince(state: State, province: Province): Boolean = {
    claims(state).collect {
      case w: WeakProvinceClaim => w.province
      case s: StrongProvinceClaim => s.province
    }.toSet.contains(province)
  }

  def hasClaimOverState(state: State, targetState: State): Boolean = {
    val set = claims(state).collect {
      case v: VassalizationClaim => v.targetState
    }.toSet
    set.contains(targetState) || actions.regions.filter(_.owner == targetState).forall(p => hasClaimOverProvince(state, p))
  }

  def addEvent(event: RelationshipEvent): Unit = {
    events ::= event
  }

  private var prevWarNames: List[String] = Nil

  def generateNewWarName(attacker: State, defender: State, target: WarTarget): String = {
    val newWarName = target match {
      case takeProvince: TakeProvince =>
        if (allClaims.exists {
          case StrongProvinceClaim(state, province) => state == attacker && province == takeProvince.province
          case _ => false
        }) {
          Localization("diplomacy.warName.liberation", takeProvince.demander.initialName, takeProvince.giver.initialName, takeProvince.province.name)
        } else {
          Localization("diplomacy.warName.conquest", takeProvince.demander.initialName, takeProvince.giver.initialName, takeProvince.province.name)
        }
      case culture: LiberateCulture =>
        Localization("diplomacy.warName.cultureLiberation", culture.demander.initialName,
          Localization(culture.culture.cultureNameKey), culture.giver.initialName)
      case state: CrackState =>
        Localization("diplomacy.warName.crackState", state.demander.initialName, state.giver.initialName)
      case money: TakeMoney =>
        Localization("diplomacy.warName.demandMoney", money.demander.initialName, money.giver.initialName)
      case vassalize: Vassalize =>
        Localization("diplomacy.warName.vassalization", vassalize.demander.initialName, vassalize.giver.initialName)
    }

    val count = prevWarNames.count(_ == newWarName)
    prevWarNames ::= newWarName
    if (count == 0) newWarName
    else s"${count + 1} $newWarName"
  }

  def improveBadBoyOverTime(): Unit = {
    _badBoy = _badBoy.map { case (k, v) =>
      val newV = if (v > BadBoyTurnRecovery) v - BadBoyTurnRecovery else 0
      k -> newV
    }.withDefaultValue(0d)
  }

  def increaseBadBoy(state: State, value: Double): Unit = {
    val current = _badBoy.getOrElse(state, 0d) + value
    _badBoy = _badBoy + (state -> current)
  }

  def addAgreement(agreement: DiplomaticAgreement): Unit = {
    val invalidAgreementsMap = agreements.groupBy(a => agreement.isPreviousAgreementBroken(a, this))
    invalidAgreementsMap.collect { case (Some(k), x) => k -> x }.foreach {
      case (breakingReason, brokenAgreements) =>
        if (breakingReason.isVoluntary) {
          breakingReason.breakers.foreach { breaker =>
            events :::= brokenAgreements.flatMap(_.voluntaryBreakingAgreementEvent(breaker, agreement.signingTurn))
          }
        }
    }

    agreements = agreement :: invalidAgreementsMap.getOrElse(None, Nil)
  }

  def sendMessage(message: DiplomaticMessage, currentTurn: Int): Unit = {
    message.beforeSendAction(this, currentTurn)
    val messages = mailbox.getOrElse(message.to, Nil)
    mailbox += message.to -> (message :: messages)

    if (currentTurn != savedTurn) {
      if (WorldConstants.Diplomacy.DeleteLastTurnEvents) {
        currentTurnMessages = Nil
      }

      savedTurn = currentTurn
    }
    currentTurnMessages ::= message
  }

  def existsMessages(states: Set[State]): Boolean = {
    states.forall(s => mailbox.get(s).forall(_.nonEmpty))
  }

  def messages(state: State, currentTurn: Int): List[DiplomaticMessage] = mailbox.getOrElse(state, Nil).
    filter(_.isPossible(this, currentTurn))

  def answerMessage(question: DiplomaticProposal, answerIsYes: Boolean, currentTurn: Int): Unit = {
    extractMessage(question, currentTurn).foreach { q =>
      if (answerIsYes) {
        q.accept(this, currentTurn)
      } else {
        q.decline(this, currentTurn)
      }
    }
  }

  private def extractMessage[T <: DiplomaticMessage](message: T, currentTurn: Int): Option[T] = {
    val (foundMessage, newMessages) = messages(message.to, currentTurn).partition(_ == message)
    mailbox += message.to -> newMessages
    foundMessage.headOption.asInstanceOf[Option[T]]
  }

  def answerMessage(declaration: DiplomaticDeclaration, currentTurn: Int): Unit = {
    extractMessage(declaration, currentTurn).foreach { m =>
      m.ok(this, currentTurn)
    }
  }

  def defaultAnswerMessage(customMessage: CustomDiplomaticQuestion, currentTurn: Int): Unit = {
    extractMessage(customMessage, currentTurn).foreach { cm =>
      cm.defaultOk(this, currentTurn)
    }
  }

  def answerDeclareWarMessage(war: DeclareWar, currentTurn: Int, allies: Set[State]): Unit = {
    extractMessage(war, currentTurn).foreach { w =>
      w.okAndCallAllies(this, currentTurn, allies)
    }
  }

  def processUnansweredMessages(state: State, currentTurn: Int): Unit = {
    messages(state, currentTurn).foreach {
      case dp: DiplomaticProposal => answerMessage(dp, false, currentTurn)
      case dd: DiplomaticDeclaration => answerMessage(dd, currentTurn)
      case c: CustomDiplomaticQuestion =>
        extractMessage(c, currentTurn).foreach { m =>
          m.defaultOk(this, currentTurn)
        }
    }
  }

  def processAllUnansweredMessages(currentTurn: Int): Unit = {
    mailbox.keySet.foreach {
      processUnansweredMessages(_, currentTurn)
    }

    mailbox = mailbox.transform { case (_, v) =>
      v.filter(_.isPossible(this, currentTurn))
    }
  }

  def agreements(from: State): List[DiplomaticAgreement] =
    agreements.filter(_.sides.contains(from))

  def agreements(sides: Set[State]): List[DiplomaticAgreement] =
    agreements.filter(a => sides.subsetOf(a.sides))

  def areAllies(first: State, second: State): Boolean = {
    agreements(Set(first, second)).exists {
      case aa: AllianceAgreement if aa.sides == Set(first, second) => true
      case _ => false
    }
  }

  def isVassal(overlord: State, vassal: State): Boolean = {
    agreements(Set(overlord, vassal)).exists {
      case va: VassalAgreement if va.overlord == overlord && va.vassal == vassal => true
      case _ => false
    }
  }

  def getOverlord(vassal: State): Option[State] = {
    agreements(vassal).collect {
      case va: VassalAgreement if va.vassal == vassal => va
    }.map(_.overlord).headOption
  }

  def getVassals(overlord: State): List[State] = {
    agreements(overlord).collect {
      case va: VassalAgreement if va.overlord == overlord => va
    }.map(_.vassal)
  }

  def events(from: State): List[RelationshipEvent] =
    events.filter(_.fromState == from)

  def events(from: State, to: State): List[RelationshipEvent] =
    events.filter(e => e.fromState == from && e.toState == to)

  def claimsBonuses(from: State): List[RelationshipBonus] = {
    claims.filter(s => s.state == from && s.targetState != from).flatMap {
      case _:VassalizationClaim => None
      case str: StrongProvinceClaim => Some(RelationshipBonus(from, str.province.owner, StrongClaimRelationshipBonus,
        Localization("diplomacy.strongClaim", from.name, str.province.name)))
      case wk: WeakProvinceClaim => Some(RelationshipBonus(from, wk.province.owner, WeakClaimRelationshipBonus,
        Localization("diplomacy.weakClaim", from.name, wk.province.name)))
    }.toList
  }

  def reputationBonuses(from: State): List[RelationshipBonus] = {
    _badBoy.filter(_._1 != from).map { case (state, bb) =>
      RelationshipBonus(from, state, (bb * BadBoyToRelationsPenalty).toInt, Localization("diplomacy.reputation"))
    }.toList
  }

  def raceAndCultureBonuses(from: State, to: State): RelationshipBonus = {
    (from.primeCulture, to.primeCulture) match {
      case (a, b) if a == b => RelationshipBonus(from, to, SameCultureRelationshipBonus, Localization("diplomacy.sameCulture", from.name, to.name))
      case (a, b) if a.race == b.race => RelationshipBonus(from, to, SameRaceRelationshipBonus, Localization("diplomacy.sameRace", from.name, to.name))
      case _ => RelationshipBonus(from, to, DifferentRaceRelationshipBonus, Localization("diplomacy.differentRace", from.name, to.name))
    }
  }

  def clearExpiredEvents(currentTurn: Int): Unit = {
    events = events.filter(_.isDurationEnded(currentTurn))
  }

  def relationships(state: State, currentTurn: Int): Map[State, Int] = {
    relationshipsDescribed(state, currentTurn).transform { case (_, list) =>
      list.map(_.bonus).sum
    }.withDefaultValue(0)
  }

  def relationshipsDescribed(state: State, currentTurn: Int): Map[State, List[RelationshipBonus]] = {
    val bonuses = events(state).map(_.relationshipsChange(currentTurn)) ++
      agreements(state).flatMap(_.relationshipBonus).filter(_.from == state) ++
      claimsBonuses(state) ++ reputationBonuses(state) ++ (states - state).map(s => raceAndCultureBonuses(state, s))
    bonuses.groupBy(_.to)
  }

  def wars: List[WarAgreement] = agreements.collect {
    case wa: WarAgreement => wa
  }

  def wars(state: State): List[WarAgreement] = agreements.collect {
    case wa: WarAgreement if wa.sides.contains(state) => wa
  }

  def addClaim(claim: ProvinceClaim): Unit = {
    val alreadyClaim = claims.collectFirst {
      case c: ProvinceClaim if c.province == claim.province && c.state == claim.state => c
    }

    (alreadyClaim, claim) match {
      case (Some(_: StrongProvinceClaim), _) =>
      // no need to add anything
      case (Some(c: WeakProvinceClaim), wk: WeakProvinceClaim) =>
        if (c.claimTurnEnd < wk.claimTurnEnd) {
          claims = claims.filterNot(_ == c) + wk
        }
      case (Some(c: WeakProvinceClaim), s: StrongProvinceClaim) =>
        claims = claims.filterNot(_ == c) + s
      case (None, c) =>
        claims += c
    }
  }

  def addClaim(claim: Claim): Unit = {
    val alreadyClaim = claims.find(_ == claim)
    if (alreadyClaim.isEmpty) {
      claims += claim
    }
  }

  def claims(state: State): List[Claim] = this.claims.filter(_.state == state).toList

  def claimsAgainst(state: State): List[Claim] = this.claims.filter(c => c.targetState == state && c.state != state).toList

  def joinWar(wa: WarAgreement, ally: State, newSide: State, currentTurn: Int): Unit = {
    if (wa.defenders.contains(ally)) {
      wa.defenders += newSide
    } else if (wa.attackers.contains(ally)) {
      wa.attackers += newSide
    } else {
      sys.error(s"Can't add $newSide to war because $ally doesn't belong in war $wa")
    }
    agreements = agreements.filterNot(_ == wa)
    addAgreement(wa)
  }

  def cancelAgreement(breaker: State, a: DiplomaticAgreement, currentTurn: Int): Unit = {
    agreements = agreements.filterNot(_ == a)
    val breakingEvents = a.voluntaryBreakingAgreementEvent(breaker, currentTurn)
    events ++= breakingEvents
  }

  private def moveBackControlToOwnersAfterWar(wa: WarAgreement, separatePeace: Boolean): Unit = {
    val remainingWars = if (separatePeace) wars else wars.filterNot(_ == wa)
    regions.filter(p => wa.sides.contains(p.owner) && p.controller != p.owner).foreach { p =>
      if (!remainingWars.exists(w => w.onDifferentSides(Set(p.owner, p.controller)))) {
        p.controller = p.owner
      }
    }
  }

  def acceptPeaceTreaty(wa: WarAgreement, targets: Set[WarTarget], currentTurn: Int): Unit = {
    moveBackControlToOwnersAfterWar(wa, separatePeace = false)

    agreements = agreements.filterNot(_ == wa)
    val newAgreements = targets.flatMap(t => applyWarTarget(wa, t, currentTurn))
    newAgreements.foreach(this.addAgreement)

    removeDisappearedStates()
  }

  def acceptSeparatePeaceTreaty(warAgreement: WarAgreement, separateState: State, targets: Set[WarTarget], currentTurn: Int): Unit = {
    val separate = separateState :: getVassals(separateState)
    if (!warAgreement.removeSides(separate.toSet, this)) {
      agreements = agreements.filterNot(_ == warAgreement)
    }

    val newAgreements = targets.flatMap(t => applyWarTarget(warAgreement, t, currentTurn))
    newAgreements.foreach(this.addAgreement)

    moveBackControlToOwnersAfterWar(warAgreement, separatePeace = true)
    increaseBadBoy(separateState, WorldConstants.Diplomacy.SeparatePeaceBadBoy)
    removeDisappearedStates()
  }

  private def applyWarTarget(warAgreement: WarAgreement, warTarget: WarTarget, currentTurn: Int): List[DiplomaticAgreement] = {
    List(warTarget).flatMap(_.validTarget(warAgreement, this)).flatMap {
      case tp: TakeProvince =>
        if (!hasClaimOverProvince(tp.demander, tp.province)) {
          increaseBadBoy(tp.demander, AnnexedProvinceWithoutClaimBadBoy)
        }
        tp.province.owner = tp.demander
        tp.province.controller = tp.demander
        Nil
      case lc: LiberateCulture =>
        increaseBadBoy(lc.demander, LiberateCultureBadBoy)
        val newState = actions.generateNewState(lc.culture, lc.demander.rulingParty)
        lc.provinces.foreach { p =>
          p.owner = newState
          p.controller = newState
        }
        Nil
      case cs: CrackState =>
        if (!hasClaimOverState(cs.demander, cs.giver)) {
          increaseBadBoy(cs.demander, CrackedStateBadBoy)
        }
        val provinces = if (cs.partiallyApplied) {
            actions.regions.filter(ow => ow.owner == cs.giver && ow.controller != ow.owner)
          } else {
            actions.states(cs.giver)
          }

        val max = provinces.maxBy(_.regionPopulation.populationCount)
        max.controller = max.owner
        provinces.filterNot(_ == max).foreach { p =>
          val newState = actions.generateNewState(p.culture, cs.demander.rulingParty)
          p.owner = newState
          p.controller = newState
        }
        Nil
      case tm: TakeMoney =>
        val money = tm.giver.budget.spendMoneyOnReparations(tm.giver.budget.moneyReserve)
        tm.demander.budget.receiveMoneyFromReparations(money)
        Nil
      case v: Vassalize =>
        if (!hasClaimOverState(v.demander, v.giver)) {
          increaseBadBoy(v.demander, VassalizedStateBadBoy)
        }
        List(new VassalAgreement(v.demander, v.giver, currentTurn))
    }
  }

  def removeDisappearedStates(): Unit = {
    val actualStates = states

    def actual(set: Set[State]): Set[State] = set & actualStates

    val agreementsToDelete = agreements.flatMap {
      case warAgreement: WarAgreement =>
        warAgreement.attackers = actual(warAgreement.attackers)
        warAgreement.defenders = actual(warAgreement.defenders)
        warAgreement.targets = warAgreement.targets.flatMap(_.validTarget(warAgreement, this))
        if (warAgreement.attackers.isEmpty || warAgreement.defenders.isEmpty || warAgreement.targets.isEmpty)
          Some(warAgreement)
        else None
      case truceAgreement: TruceAgreement =>
        if (truceAgreement.sides != actual(truceAgreement.sides)) Some(truceAgreement)
        else None
      case vassalAgreement: VassalAgreement =>
        if (vassalAgreement.sides != actual(vassalAgreement.sides)) Some(vassalAgreement)
        else None
      case allianceAgreement: AllianceAgreement =>
        if (allianceAgreement.sides != actual(allianceAgreement.sides)) Some(allianceAgreement)
        else None
    }.toSet

    agreements = agreements.filterNot(agreementsToDelete.contains)

    regions.foreach { p =>
      if (!actualStates.contains(p.controller)) {
        p.controller = p.owner
      }

      val lostWarriors = p.regionWarriors.allWarriors.filterNot(w => actualStates.contains(w.owner))
      p.regionWarriors.takeWarriors(lostWarriors)
    }

    events = events.filter(e => actual(Set(e.fromState, e.toState)).size == 2)

    _badBoy = _badBoy.filterKeys(actualStates.contains)

    mailbox = mailbox.filterKeys(actualStates.contains)

    claims = claims.filter(s => actualStates.contains(s.state))
  }

  def possibleVassalizationWarTargets(state: State): List[State] = {
    state :: getVassals(state)
  }

  def neighbourProvincesToTake(attacker:State, defender: State): List[Province] = {
    val attackerRegionsNeighbours = regions.filter(_.owner == attacker).flatMap(_.neighbours).toSet
    val defenderRegions = regions.filter(_.owner == defender).toSet
    (attackerRegionsNeighbours & defenderRegions).toList
  }

  def possibleCulturesToLiberate(state: State): Set[Culture] = {
    regions.filter(_.owner == state).map(_.culture).toSet - state.primeCulture
  }

  def possibleTargetsForStartingWar(attacker:State, defender:State):Set[WarTarget] = {
    possibleVassalizationWarTargets(defender).map { d =>
      Vassalize(attacker, d)
    } ++ neighbourProvincesToTake(attacker, defender).map { p =>
      TakeProvince(attacker, defender, p)
    } ++ possibleCulturesToLiberate(defender).map {c =>
      val provinces = regions.filter(_.owner == defender).filter(_.culture == c)
      LiberateCulture(attacker, defender, c, provinces.toSet)
    } ++ List(CrackState(attacker, defender, false), TakeMoney(attacker, defender))
  }.toSet

  def possibleWarTargets(warAgreement: WarAgreement, state: State): Set[WarTarget] = {
    val enemies = warAgreement.oppositeSideByState(state)
    val allies = warAgreement.sideByState(state)
    val possible:Set[WarTarget] = enemies.flatMap { en =>
      List(
        CrackState(state, en, false),
        Vassalize(state, en),
        TakeMoney(state, en)) ++
        regions.filter(r => r.owner == en && allies.contains(r.controller)).map { r =>
          TakeProvince(state, en, r)
        } ++
        regions.filter(r => r.owner == en && r.culture != en.primeCulture).groupBy(_.culture).map {
          case (c, r) => LiberateCulture(state, en, c, r.toSet)
        }
    }

    (possible -- warAgreement.targets).filter { target =>
      WarTarget.areWarTargetsConsistent(warAgreement.targets + target)
    }
  }

  def addWarTarget(war: WarAgreement, warTarget: WarTarget): Unit = {
    if (possibleWarTargets(war, warTarget.demander).contains(warTarget)) {
      war.targets += warTarget
    }
  }


  def resolveStalledWars(): Unit = {
    wars.foreach { war =>
      val lastBattle = if (war.battles.nonEmpty) {
        war.battles.maxBy(_.turn).turn
      } else war.startingTurn

      if (actions.turn - lastBattle > WorldConstants.Diplomacy.StalledWarTimeUntilPeace) {
        resolveStalledWar(war)
      }
    }
  }

  def resolveStalledWar(war: WarAgreement): Unit = {
    war.targets.filter {
      case TakeProvince(demander, giver, province) =>
        war.sideByState(demander).contains(province.controller)
      case WarAgreement.LiberateCulture(demander, giver, culture, provinces) =>
        provinces.exists(p => war.sideByState(demander).contains(p.controller))
      case WarAgreement.CrackState(demander, giver, _) =>
        actions.states(giver).exists(p => war.sideByState(demander).contains(p.controller))
      case _:WarAgreement.TakeMoney => false
      case _:WarAgreement.Vassalize => false
    }.foreach {
      case tp:TakeProvince => applyWarTarget(war, tp, actions.turn)
      case LiberateCulture(demander, giver, culture, provinces) =>
        val controlled = provinces.filter(p => war.sideByState(demander).contains(p.controller))
        val newTarget = LiberateCulture(demander, giver, culture, controlled)
        applyWarTarget(war, newTarget, actions.turn)
      case cs:CrackState =>
        applyWarTarget(war, cs.copy(partiallyApplied = true), actions.turn)
    }

  }

}

object WorldDiplomacy {

  case class RelationshipBonus(from: State, to: State, bonus: Int, title: String)

}

sealed trait Claim {
  def state: State

  def targetState: State
}

object Claim {

  sealed trait ProvinceClaim extends Claim {
    val state: State
    val province: Province
  }

  case class StrongProvinceClaim(state: State, province: Province) extends ProvinceClaim {
    override def targetState: State = province.owner
  }

  case class WeakProvinceClaim(state: State, province: Province, claimTurnEnd: Int) extends ProvinceClaim {
    override def targetState: State = province.owner
  }

  case class VassalizationClaim(state: State, possibleVassal: State, claimTurnEnd: Int) extends Claim {
    override def targetState: State = possibleVassal
  }

}

