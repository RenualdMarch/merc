package mr.merc.diplomacy

import mr.merc.diplomacy.Claim.{ProvinceClaim, VassalizationClaim}
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement._
import mr.merc.diplomacy.DiplomaticMessage._
import mr.merc.economics.{WorldConstants, WorldStateDiplomacyActions}
import mr.merc.log.Logging
import mr.merc.politics.State

import scala.util.Random

class DiplomaticAI(state: State, actions: WorldStateDiplomacyActions) extends Logging {

  import WorldConstants.AI._

  val situation: DiplomaticSituation = new DiplomaticSituation(actions.diplomacyEngine)

  def aiMove(onlyAnswer: Boolean, random: Random = Random): Unit = {
    answerMessages()
    if (!onlyAnswer) {
      sendMessages(random)
    }
  }

  def answerMessages(): Unit = {
    actions.mailbox(state).foreach {
      case m: DiplomaticDeclaration => actions.acknowledgeMessage(m)
      case v: VassalizationProposal => actions.answerMessage(v, answerVassalizationProposal(v))
      case ajw: AskJoinWar => actions.answerMessage(ajw, answerAskJoinWar(ajw))
      case pp: ProposePeace => actions.answerMessage(pp, answerProposePeace(pp))
      case ap: AllianceProposal => actions.answerMessage(ap, answerAllianceProposal(ap))
      case op: OverlordshipProposal => actions.answerMessage(op, answerOverlordshipProposal(op))
      case dw: DeclareWar => actions.answerDeclareWar(dw, answerDeclareWar(dw))
    }
  }

  def answerVassalizationProposal(message: VassalizationProposal): Boolean = {
    if (!situation.shareBorder(state, message.from)) false
    else if (actions.relationships(state)(message.from) < MinRelationshipForVassalization) false
    else if (situation.areRivals(state, message.from)) false
    else situation.powerDifference(message.from, state) > PowerDifferenceForVassalization
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

  def answerPeaceBegging(message: ProposePeace, stateSide: Set[State], enemySide: Set[State]): Boolean = {
    val stateTargets = message.warAgreement.targets.filter(t => stateSide.contains(t.demander))
    message.acceptedTargets == stateTargets
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
      case cs: CrackState => enemyArmyDomination && actions.regions.filter(_.owner == cs.giver).forall { p =>
        enemySide.contains(p.controller)
      }
    }
  }

  def answerAllianceProposal(message: AllianceProposal): Boolean = {
    if (situation.areRivals(message.from, state)) false
    else if (situation.areRivalsOfRivals(message.from, state)) true
    else actions.relationships(state)(message.from) >= MinRelationshipToStartAlliance
  }

  def answerOverlordshipProposal(message: OverlordshipProposal): Boolean = {
    if (!situation.shareBorder(state, message.from)) false
    else if (actions.relationships(state)(message.from) < MinRelationshipForVassalization) false
    else if (situation.areRivals(state, message.from)) false
    else situation.powerDifference(state, message.from) > PowerDifferenceForVassalization
  }

  def answerDeclareWar(message: DeclareWar): Set[State] = actions.allies(state).toSet

  def sendMessages(random: Random = Random): Unit = {
    random.shuffle(possibleMessages()).take(WorldConstants.AI.MaxMessagesPerTurn).foreach { m =>
      actions.sendMessage(m)
    }
  }

  def possibleMessages(): List[DiplomaticMessage] = {
    declareWar() ::: proposeAlliance() ::: proposeVassalization() ::: proposeOverlordship() filter(
      _.isPossible(actions.diplomacyEngine, actions.turn))
  }

  private def declareWar():List[DeclareWar] = {
    if (situation.inWar(state).nonEmpty) Nil
    else {
      val weakNeigs = weakerNeighbours
      val rivalWar = weakNeigs.find(s => situation.areRivals(state, s)).flatMap { rival =>
        val claims = actions.claims(state).filter(_.targetState == rival)
        val vassalizationOpt = claims.collectFirst{ case v:VassalizationClaim => v}
        val provincesOpt = claims.collectFirst { case p:ProvinceClaim if situation.neighbouringProvince(state, p.province) => p}
        val target = vassalizationOpt match {
          case Some(v) => Some(new Vassalize(state, rival))
          case None => provincesOpt.map(c => new TakeProvince(state, rival, c.province))
        }

        target.map { t =>
          new DeclareWar(state, rival, t, actions.allies(state).toSet)
        }
      }
      rivalWar match {
        case Some(war) => List(war)
        case None => situation.neighbouringProvinces(state).find { p =>
          weakNeigs.contains(p.owner) && p.owner == p.controller
        }.map { p =>
          new DeclareWar(state, p.owner, new TakeProvince(state, p.owner, p), actions.allies(state).toSet)
        }.toList
      }

    }
  }

  private def weakerNeighbours:List[State] = {
    situation.neighbours(state).filter { s =>
      situation.stateArmyTotalLevel(state) > PowerDifferenceToStartWar * situation.stateArmyTotalLevel(s)
    }
  }

  private def proposeAlliance():List[AllianceProposal] = {
    val otherStates = actions.regions.map(_.owner).toSet - state
    otherStates.filter { s =>
      situation.areRivalsOfRivals(state, s) || actions.relationships(state)(s) > MinRelationshipToStartAlliance
    }.map(a => new AllianceProposal(state, a)).toList
  }

  private def proposeVassalization():List[VassalizationProposal] = {
    val otherStates = actions.regions.map(_.owner).toSet - state
    otherStates.filter { s =>
      actions.relationships(s)(state) > MinRelationshipForVassalization &&
        situation.shareBorder(state, s) && s.primeCulture != state.primeCulture &&
      situation.powerDifference(state, s) > PowerDifferenceForVassalization
    }.map(s => new VassalizationProposal(state, s)).toList
  }

  private def proposeOverlordship():List[OverlordshipProposal] = {
    val otherStates = actions.regions.map(_.owner).toSet - state

    val needsOverlord = situation.rivals(state).exists { s =>
      situation.powerDifference(s, state) > PowerDifferenceForVassalization
    }

    if (needsOverlord) {
      otherStates.filter { s =>
        situation.shareBorder(state, s) && !situation.areRivals(state, s)
          situation.powerDifference(s, state) > PowerDifferenceForVassalization
      }.map(s => new OverlordshipProposal(state, s)).toList
    } else Nil
  }
}

