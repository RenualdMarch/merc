package mr.merc.diplomacy

import mr.merc.diplomacy.Claim.{ProvinceClaim, VassalizationClaim}
import mr.merc.diplomacy.DiplomaticAgreement.{AllianceAgreement, FriendshipAgreement, SanctionAgreement, WarAgreement}
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement._
import mr.merc.diplomacy.DiplomaticMessage._
import mr.merc.economics.{WorldConstants, WorldStateDiplomacyActions}
import mr.merc.log.Logging
import mr.merc.politics.State

import scala.util.Random

class DiplomaticAI(state: State, actions: WorldStateDiplomacyActions) extends Logging {

  import WorldConstants.AI._

  val situation: DiplomaticSituation = actions.situation

  def aiMove(onlyAnswer: Boolean, random: Random = Random): Unit = {
    answerMessages()
    if (!onlyAnswer) {
      sendMessages(random)
    }
  }

  def answerMessages(): Unit = {
    actions.mailbox(state).foreach {
      case m: DiplomaticDeclaration =>
        actions.acknowledgeMessage(m)
        m match {
          case s: SanctionsEnacted =>
            actions.sendMessage(SanctionsEnacted(state, s.from))
          case _ => // do nothing
        }
      case v: VassalizationProposal => actions.answerMessage(v, answerVassalizationProposal(v))
      case ajw: AskJoinWar => actions.answerMessage(ajw, answerAskJoinWar(ajw))
      case pp: ProposePeace => actions.answerMessage(pp, answerProposePeace(pp))
      case sp: ProposeSeparatePeace => actions.answerMessage(sp, answerProposeSeparatePeace(sp))
      case ap: AllianceProposal => actions.answerMessage(ap, answerAllianceProposal(ap))
      case op: OverlordshipProposal => actions.answerMessage(op, answerOverlordshipProposal(op))
      case dw: DeclareWar => actions.answerDeclareWar(dw, answerDeclareWar(dw))
      case vd: VassalizationDemand => actions.answerMessage(vd, answerVassalizationDemand(vd))
      case fp: FriendshipProposal => actions.answerMessage(fp, answerFriendshipProposal(fp))
    }
  }

  def rethinkSanctions(): Unit = {
    actions.agreements(state).collect {
      case s: SanctionAgreement => s
    }.foreach { sa =>
      if (!situation.areRivals(state, sa.underSanctions)) {
        actions.sendMessage(SanctionsStopped(state, sa.underSanctions))
      }
    }
    situation.rivals(state).foreach { r =>
      actions.sendMessage(SanctionsEnacted(state, r))
    }
  }

  def answerVassalizationProposal(message: VassalizationProposal): Boolean = {
    if (!situation.shareBorder(state, message.from)) false
    else if (actions.relationships(state)(message.from) < MinRelationshipForVassalization) false
    else if (situation.areRivals(state, message.from)) false
    else situation.powerDifference(message.from, state) > PowerDifferenceForVassalization
  }

  def answerVassalizationDemand(message: VassalizationDemand): Boolean = {
    if (!situation.shareBorder(state, message.from)) false
    else situation.powerDifference(message.from, state) > PowerDifferenceForStoppingBeingAVassal
  }

  def answerAskJoinWar(message: AskJoinWar): Boolean = {
    val enemy = (Set(message.warAgreement.warVictim, message.warAgreement.warInitiator) - message.from).head
    if (situation.areRivals(state, enemy)) true
    else if (actions.relationships(state)(enemy) < RelationshipForHate) true
    else if (actions.relationships(state)(message.from) < MinRelationshipForHelpAlly) false
    else situation.powerDifference(enemy, message.from) < EnemyPowerDifferenceForHelpAlly
  }

  def answerProposePeace(message: ProposePeace): Boolean = {
    val war = message.warAgreement
    val (stateSide, enemySide) = if (war.attackers.contains(state)) (war.attackers, war.defenders)
    else if (war.defenders.contains(state)) (war.defenders, war.attackers)
    else {
      error(s"state $state is not in war $war")
      return false
    }

    if (message.acceptedTargets.isEmpty) answerWhitePeace(message, stateSide, enemySide)
    else if (message.acceptedTargets.exists(at => stateSide.contains(at.giver))) answerPeaceDemand(message, stateSide, enemySide)
    else if (message.acceptedTargets.exists(at => enemySide.contains(at.giver))) answerPeaceBegging(message, stateSide, enemySide)
    else {
      error(s"Can't decide what to do with peace proposal $message in war $war")
      false
    }
  }

  def answerProposeSeparatePeace(message: ProposeSeparatePeace): Boolean = {
    val war = message.warAgreement
    val (stateSide, enemySide) = if (war.attackers.contains(state)) (war.attackers, war.defenders)
    else if (war.defenders.contains(state)) (war.defenders, war.attackers)
    else {
      error(s"state $state is not in war $war")
      return false
    }

    if (message.acceptedTargets.isEmpty) answerSeparateWhitePeace(message, stateSide, enemySide)
    else if (message.acceptedTargets.exists(at => stateSide.contains(at.giver))) answerSeparatePeaceDemand(message, stateSide, enemySide)
    else if (message.acceptedTargets.exists(at => enemySide.contains(at.giver))) answerSeparatePeaceBegging(message, stateSide, enemySide)
    else {
      error(s"Can't decide what to do with peace proposal $message in war $war")
      false
    }
  }

  def answerSeparateWhitePeace(message: ProposeSeparatePeace, stateSide: Set[State], enemySide: Set[State]): Boolean = {
    // accept only if enemy controls smth and has bigger army
    val stateSideTotalArmy = stateSide.map(s => situation.stateArmyTotalLevel(s)).sum
    val enemySideTotalArmy = enemySide.map(s => situation.stateArmyTotalLevel(s)).sum

    val enemyArmyDomination = stateSideTotalArmy == 0 || enemySideTotalArmy / stateSideTotalArmy > PowerDifferenceForWhitePeace

    val enemyHasControl = actions.regions.exists { p =>
      p.owner == state && enemySide.contains(p.controller)
    }

    enemyArmyDomination && enemyHasControl
  }

  def answerWhitePeace(message: ProposePeace, stateSide: Set[State], enemySide: Set[State]): Boolean = {
    val stateSideTotalArmy = stateSide.map(s => situation.stateArmyTotalLevel(s)).sum
    val enemySideTotalArmy = enemySide.map(s => situation.stateArmyTotalLevel(s)).sum

    val enemyArmyDomination = stateSideTotalArmy == 0 || enemySideTotalArmy / stateSideTotalArmy > PowerDifferenceForWhitePeace
    val noEnemyControl = !actions.regions.exists { p =>
      stateSide.contains(p.owner) && enemySide.contains(p.controller)
    }

    val noStateControl = !actions.regions.exists { p =>
      enemySide.contains(p.owner) && stateSide.contains(p.controller)
    }

    enemyArmyDomination && noEnemyControl && noStateControl
  }

  def answerSeparatePeaceBegging(message: ProposeSeparatePeace, stateSide: Set[State], enemySide: Set[State]): Boolean = {
    false
  }

  def answerPeaceBegging(message: ProposePeace, stateSide: Set[State], enemySide: Set[State]): Boolean = {
    val stateTargets = message.warAgreement.targets.filter(t => stateSide.contains(t.demander))
    message.acceptedTargets == stateTargets
  }

  def answerSeparatePeaceDemand(message: ProposeSeparatePeace, stateSide: Set[State], enemySide: Set[State]): Boolean = {
    val stateSideTotalArmy = stateSide.map(s => situation.stateArmyTotalLevel(s)).sum
    val enemySideTotalArmy = enemySide.map(s => situation.stateArmyTotalLevel(s)).sum

    val enemyArmyDomination = stateSideTotalArmy == 0 || enemySideTotalArmy / stateSideTotalArmy > PowerDifferenceForWhitePeace

    val stateAndVassals = state :: actions.diplomacyEngine.getVassals(state)

    message.acceptedTargets.forall {
      case tp: TakeProvince => enemySide.contains(tp.province.controller) && stateAndVassals.contains(tp.province.owner)
      case lc: LiberateCulture => actions.provincesByCulture(lc.giver, lc.culture).forall { p =>
        enemySide.contains(p.controller) && stateAndVassals.contains(p.owner)
      }
      case tm: TakeMoney => enemyArmyDomination
      case v: Vassalize => enemyArmyDomination && actions.regions.filter(_.owner == v.giver).forall { p =>
        enemySide.contains(p.controller) && stateAndVassals.contains(p.owner)
      }
    }
  }

  def answerPeaceDemand(message: ProposePeace, stateSide: Set[State], enemySide: Set[State]): Boolean = {
    val stateSideTotalArmy = stateSide.map(s => situation.stateArmyTotalLevel(s)).sum
    val enemySideTotalArmy = enemySide.map(s => situation.stateArmyTotalLevel(s)).sum

    val enemyArmyDomination = stateSideTotalArmy == 0 || enemySideTotalArmy / stateSideTotalArmy > PowerDifferenceForWhitePeace

    message.acceptedTargets.forall {
      case tp: TakeProvince => enemySide.contains(tp.province.controller) && stateSide.contains(tp.province.owner)
      case lc: LiberateCulture => actions.provincesByCulture(lc.giver, lc.culture).forall { p =>
        enemySide.contains(p.controller) && stateSide.contains(p.owner)
      }
      case tm: TakeMoney => enemyArmyDomination
      case v: Vassalize => enemyArmyDomination && actions.regions.filter(_.owner == v.giver).forall { p =>
        enemySide.contains(p.controller)
      }
    }
  }

  def answerAllianceProposal(message: AllianceProposal): Boolean = {
    val relations = actions.relationships(state)(message.from)

    if (situation.areRivals(message.from, state)) false
    else relations >= MinRelationshipToStartAlliance
  }

  def answerFriendshipProposal(message: FriendshipProposal): Boolean = {
    actions.relationships(state)(message.from) > MinRelationshipForStartFriendship
  }

  def answerOverlordshipProposal(message: OverlordshipProposal): Boolean = {
    if (!situation.shareBorder(state, message.from)) false
    else if (actions.relationships(state)(message.from) < MinRelationshipForVassalization) false
    else if (situation.areRivals(state, message.from)) false
    else situation.powerDifference(state, message.from) > PowerDifferenceForVassalization
  }

  def answerDeclareWar(message: DeclareWar): Set[State] = actions.allies(state).toSet

  def sendMessages(random: Random = Random): Unit = {
    rethinkSanctions()
    random.shuffle(possibleMessages()).take(WorldConstants.AI.MaxMessagesPerTurn).foreach { m =>
      actions.sendMessage(m)
    }
  }

  def possibleMessages(): List[DiplomaticMessage] = {
    (declareWar() ::: proposeAlliance() ::: proposeVassalization() ::: proposeOverlordship() :::
      demandFreedom() ::: proposePeace() ::: proposeFriendship() ::: breakFriendshipTreaty() :::
      breakAllianceTreaty() ::: releaseVassal()).
      filter(_.isPossible(actions.diplomacyEngine, actions.turn))
  }

  private def proposePeace(): List[DiplomaticProposal] = {
    actions.diplomacyEngine.wars(state).flatMap { war =>
      val stateSide = war.sideByState(state)
      val enemySide = war.oppositeSideByState(state)
      val stateSideTotalArmy = stateSide.map(s => situation.stateArmyTotalLevel(s)).sum
      val enemySideTotalArmy = enemySide.map(s => situation.stateArmyTotalLevel(s)).sum

      if (stateSideTotalArmy <= TotalArmySumToBeTiredOfWar && enemySideTotalArmy <= TotalArmySumToBeTiredOfWar) {
        // both sides are tired
        if (war.isLeader(state, actions.diplomacyEngine)) List(proposePeaceOfCurrentAchieved(war))
        else Nil
      } else if (stateSideTotalArmy <= TotalArmySumToBeTiredOfWar && enemySideTotalArmy >= TotalArmySumToBeTiredOfWar) {
        // we cant attack any more, enemy can
        if (war.isLeader(state, actions.diplomacyEngine)) List(proposePeaceOfCurrentAchieved(war))
        else List(proposeSeparatePeaceOfCurrentAchieved(war))
      } else if (stateSideTotalArmy >= TotalArmySumToBeTiredOfWar && enemySideTotalArmy <= TotalArmySumToBeTiredOfWar) {
        // we are winning
        if (war.isLeader(state, actions.diplomacyEngine)) List(proposePeaceWithMaxDemands(war))
        else Nil
      } else if (allTargetsReached(war)) {
        List(proposePeaceWithMaxDemands(war))
      } else Nil
    }
  }

  private def proposePeaceOfCurrentAchieved(war: WarAgreement): ProposePeace = {
    val targets = war.targets.filter(t => isAchieved(war, t))
    ProposePeace(state, war.oppositeLeader(state, actions.diplomacyEngine), war, targets)
  }

  private def isAchieved(war: WarAgreement, target: WarTarget): Boolean = {
    val demanderSide = war.sideByState(target.demander)
    target match {
      case TakeProvince(_, _, province) =>
        demanderSide.contains(province.controller)
      case LiberateCulture(_, _, _, provinces) =>
        provinces.forall(p => demanderSide.contains(p.controller))
      case TakeMoney(_, _) => true
      case Vassalize(_, giver) =>
        actions.regions.filter(_.owner == giver).forall(p => demanderSide.contains(p.controller))
    }
  }

  private def proposeSeparatePeaceOfCurrentAchieved(war: WarAgreement): ProposeSeparatePeace = {
    val targets = war.targets.filter(t => t.demander == state || t.giver == state)
    ProposeSeparatePeace(state, war.oppositeLeader(state, actions.diplomacyEngine), war, targets, state)
  }

  private def proposePeaceWithMaxDemands(war: WarAgreement): ProposePeace = {
    val side = war.sideByState(state)
    val targets = war.targets.filter(t => side.contains(t.demander))
    ProposePeace(state, war.oppositeLeader(state, actions.diplomacyEngine), war, targets)
  }

  private def allTargetsReached(war: WarAgreement): Boolean = {
    val demanderSide = war.sideByState(state)
    war.targets.filter(wt => demanderSide.contains(wt.demander)).forall(wt => isAchieved(war, wt))
  }

  private def declareWar(): List[DeclareWar] = {
    val minRels = MinRelationsToStartWar(state.elites.stateRuler.aggressiveness)

    if (situation.inWar(state).nonEmpty) Nil
    else {
      val weakNeigs = weakerNeighbours()
      val rivalWar = weakNeigs.filter(s => situation.areRivals(state, s)).filter { s =>
        actions.diplomacyEngine.relationships(state, actions.turn)(s) < minRels
      }.flatMap { rival =>
        val claims = actions.claims(state).filter(_.targetState == rival)
        val vassalizationOpt = claims.collectFirst { case v: VassalizationClaim => v }
        val provincesOpt = claims.collectFirst { case p: ProvinceClaim if situation.neighbouringProvince(state, p.province) => p }
        val target = vassalizationOpt match {
          case Some(v) => Some(Vassalize(state, rival))
          case None => provincesOpt.map(c => TakeProvince(state, rival, c.province))
        }

        target.map { t =>
          new DeclareWar(state, rival, t, actions.allies(state).toSet)
        }
      }.headOption

      rivalWar match {
        case Some(war) => List(war)
        case None => situation.neighbouringProvinces(state).filter { p =>
          weakNeigs.contains(p.owner) && p.owner == p.controller
        }.filter { p =>
          actions.diplomacyEngine.relationships(state, actions.turn)(p.owner) < minRels
        }.map { p =>
          new DeclareWar(state, p.owner, TakeProvince(state, p.owner, p), actions.allies(state).toSet)
        }.headOption.toList
      }
    }
  }

  // aggressiveness
  // 100 -> 1
  // 0 -> PowerDifferenceToStartWar
  private def weakerNeighbours(): List[State] = {
    val powerDifferenceToStartWar = state.elites.stateRuler.aggressiveness * (1 - PowerDifferenceToStartWar) / 100d + PowerDifferenceToStartWar
    situation.neighbours(state).filter { s =>
      situation.stateArmyTotalLevel(state) > powerDifferenceToStartWar * situation.stateArmyTotalLevel(s)
    }
  }

  private def proposeAlliance(): List[AllianceProposal] = {
    val otherStates = actions.regions.map(_.owner).toSet - state
    otherStates.filter { s =>
      !situation.isInWar(s) && (situation.areRivalsOfRivals(state, s) &&
        actions.relationships(state)(s) > MinRelationshipToStartAlliance)
    }.map(a => new AllianceProposal(state, a)).toList
  }

  private def proposeVassalization(): List[VassalizationProposal] = {
    val otherStates = actions.regions.map(_.owner).toSet - state
    otherStates.filter { s =>
      actions.relationships(s)(state) > MinRelationshipForVassalization &&
        situation.shareBorder(state, s) && s.primeCulture != state.primeCulture &&
        situation.powerDifference(state, s) > PowerDifferenceForVassalization
    }.map(s => new VassalizationProposal(state, s)).toList
  }

  private def proposeOverlordship(): List[OverlordshipProposal] = {
    val otherStates = actions.regions.map(_.owner).toSet - state

    val needsOverlord = situation.rivals(state).exists { s =>
      situation.powerDifference(s, state) > PowerDifferenceForVassalization
    }

    if (needsOverlord) {
      otherStates.filter { s =>
        actions.relationships(state)(s) > MinRelationshipForVassalization &&
          situation.shareBorder(state, s) && !situation.areRivals(state, s)
        situation.powerDifference(s, state) > PowerDifferenceForVassalization
      }.map(s => new OverlordshipProposal(state, s)).toList
    } else Nil
  }

  private def releaseVassal(): List[ReleaseVassal] = Nil

  private def demandFreedom(): List[StopBeingVassal] = {
    actions.diplomacyEngine.getOverlord(state).toList.filter { overlord =>
      situation.powerDifference(overlord, state) < PowerDifferenceForStoppingBeingAVassal
    }.map { overlord =>
      new StopBeingVassal(state, overlord)
    }
  }

  private def proposeFriendship(): List[FriendshipProposal] = {
    val neigs = situation.neighbours(state)
    neigs.diff(weakerNeighbours()).diff(situation.rivals(state)).filter { st =>
      actions.diplomacyEngine.relationships(state, actions.turn)(st) > MinRelationshipForStartFriendship
    }.diff(proposeAlliance().map(_.to)).map { st =>
      new FriendshipProposal(state, st)
    }
  }

  private def breakFriendshipTreaty(): List[BreakFriendshipTreaty] = {
    actions.agreements(state).collect {
      case fr: FriendshipAgreement => fr.sides - state
    }.flatten.filter { st =>
      actions.diplomacyEngine.relationships(state, actions.turn)(st) < MaxRelationshipToStopFriendship
    }.map { st =>
      new BreakFriendshipTreaty(state, st)
    }
  }

  private def breakAllianceTreaty(): List[BreakAllianceTreaty] = {
    actions.agreements(state).collect {
      case aa: AllianceAgreement => aa.sides - state
    }.flatten.filter { st =>
      actions.diplomacyEngine.relationships(state, actions.turn)(st) < MaxRelationshipToStopAlliance
    }.map { st =>
      new BreakAllianceTreaty(state, st)
    }
  }

}

