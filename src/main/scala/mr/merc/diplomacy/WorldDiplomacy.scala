package mr.merc.diplomacy

import mr.merc.diplomacy.DiplomaticAgreement.{AllianceAgreement, VassalAgreement, WarAgreement}
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement._
import mr.merc.diplomacy.Claim.{ProvinceClaim, StrongProvinceClaim, WeakProvinceClaim}
import mr.merc.diplomacy.DiplomaticMessage.DeclareWar
import mr.merc.diplomacy.WorldDiplomacy.RelationshipBonus
import mr.merc.politics.{Province, State}
import mr.merc.economics.WorldConstants.Diplomacy._
import mr.merc.economics.WorldStateDiplomacyActions
import mr.merc.local.Localization


class WorldDiplomacy(actions:WorldStateDiplomacyActions) {
  def regions:List[Province] = actions.regions

  private def states:Set[State] = regions.toSet.map{p:Province => p.owner}

  private var agreements: List[DiplomaticAgreement] = Nil
  private var events: List[RelationshipEvent] = Nil

  private var mailbox:Map[State, List[DiplomaticMessage]] = Map()

  private var claims: List[Claim] = Nil

  def addEvent(event: RelationshipEvent): Unit = {
    events ::= event
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

  def sendMessage(message: DiplomaticMessage, currentTurn:Int): Unit = {
    message.beforeSendAction(this, currentTurn)
    val messages = mailbox.getOrElse(message.to, Nil)
    mailbox += message.to -> (message :: messages)
  }

  def messages(state:State, currentTurn:Int):List[DiplomaticMessage] = mailbox.getOrElse(state, Nil).filter(_.isPossible(this, currentTurn))

  def answerMessage(question:DiplomaticProposal, answerIsYes:Boolean, currentTurn:Int): Unit = {
    extractMessage(question, currentTurn).foreach { q =>
      if (answerIsYes) {
        q.accept(this, currentTurn)
      } else {
        q.decline(this, currentTurn)
      }
    }
  }

  private def extractMessage[T <: DiplomaticMessage](message: T, currentTurn:Int):Option[T] = {
    val (foundMessage, newMessages) = messages(message.to, currentTurn).partition(_ == message)
    mailbox += message.to -> newMessages
    foundMessage.headOption.asInstanceOf[Option[T]]
  }

  def answerMessage(declaration:DiplomaticDeclaration, currentTurn:Int): Unit = {
    extractMessage(declaration, currentTurn).foreach(_.ok(this, currentTurn))
  }

  def defaultAnswerMessage(customMessage:CustomDiplomaticQuestion, currentTurn:Int): Unit = {
    extractMessage(customMessage, currentTurn).foreach { cm =>
      cm.defaultOk(this, currentTurn)
    }
  }

  def answerDeclareWarMessage(war:DeclareWar, currentTurn:Int, allies:Set[State]): Unit = {
    extractMessage(war, currentTurn).foreach { w =>
      w.okAndCallAllies(this, currentTurn, allies)
    }

  }

  def processUnansweredMessages(state:State, currentTurn:Int): Unit = {
    messages(state, currentTurn).foreach {
      case dp:DiplomaticProposal => answerMessage(dp, false, currentTurn)
      case dd:DiplomaticDeclaration => answerMessage(dd, currentTurn)
      case c:CustomDiplomaticQuestion => c.defaultOk(this, currentTurn)
    }
  }

  def processAllUnansweredMessages(currentTurn:Int): Unit = {
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

  def areAllies(first:State, second:State):Boolean = {
    agreements(Set(first, second)).exists {
      case aa:AllianceAgreement if aa.sides == Set(first, second) => true
      case _ => false
    }
  }

  def isVassal(overlord:State, vassal:State):Boolean = {
    agreements(Set(overlord, vassal)).exists {
      case va:VassalAgreement if va.overlord == overlord && va.vassal == vassal => true
      case _ => false
    }
  }

  def getOverlord(vassal:State):Option[State] = {
    agreements(vassal).collect {
      case va:VassalAgreement if va.vassal == vassal => va
    }.map(_.overlord).headOption
  }

  def getVassals(overlord:State):List[State] = {
    agreements(overlord).collect {
      case va:VassalAgreement if va.overlord == overlord => va
    }.map(_.vassal)
  }

  def events(from: State): List[RelationshipEvent] =
    events.filter(_.fromState == from)

  def events(from: State, to: State): List[RelationshipEvent] =
    events.filter(e => e.fromState == from && e.toState == to)

  def claims(from: State): List[RelationshipBonus] = {
    claims.filter(_.state == from).map {
      case str: StrongProvinceClaim => RelationshipBonus(from, str.province.owner, StrongClaimRelationshipBonus,
        Localization("diplomacy.strongClaim", from.name, str.province.name))
      case wk: WeakProvinceClaim => RelationshipBonus(from, wk.province.owner, WeakClaimRelationshipBonus,
        Localization("diplomacy.weakClaim", from.name, wk.province.name))
    }
  }

  def raceAndCultureBonuses(from:State, to:State):RelationshipBonus = {
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
      claims(state) ++ (states - state).map(s => raceAndCultureBonuses(state, s))
    bonuses.groupBy(_.to)
  }

  def wars: List[WarAgreement] = agreements.collect {
    case wa: WarAgreement => wa
  }

  def wars(state: State): List[WarAgreement] = agreements.collect {
    case wa: WarAgreement if wa.sides.contains(state) => wa
  }

  def addClaim(claim:ProvinceClaim): Unit = {
    val alreadyClaim = claims.collectFirst {
      case c:ProvinceClaim if c.province == claim.province && c.state == claim.state => c
    }

    (alreadyClaim, claim) match {
      case (Some(_:StrongProvinceClaim), _) =>
        // no need to add anything
      case (Some(c:WeakProvinceClaim), wk:WeakProvinceClaim) =>
        if (c.claimTurnEnd < wk.claimTurnEnd) {
          claims = wk :: claims.filterNot(_ == c)
        }
      case (Some(c:WeakProvinceClaim), s:StrongProvinceClaim) =>
        claims = s :: claims.filterNot(_ == c)
      case (None, c) =>
        claims ::= c
    }
  }

  def addClaim(claim:Claim): Unit = {
    val alreadyClaim = claims.find(_ == claim)
    if (alreadyClaim.isEmpty) {
      claims ::= claim
    }
  }

  def joinWar(wa:WarAgreement, ally: State, newSide:State, currentTurn:Int): Unit = {
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

  def cancelAgreement(breaker:State, a:DiplomaticAgreement, currentTurn:Int): Unit = {
    agreements = agreements.filterNot(_ == a)
    val breakingEvents = a.voluntaryBreakingAgreementEvent(breaker, currentTurn)
    events ++= breakingEvents
  }

  def addWarTarget(wa:WarAgreement, wt:WarTarget, currentTurn:Int): Unit = {
    wa.targets += wt
  }

  private def moveBackControlToOwnersAfterWar(wa:WarAgreement): Unit = {
    val remainingWars = wars.filterNot(_ == wa)
    regions.filter(p => wa.sides.contains(p.owner) && p.controller != p.owner).foreach {p =>
      if (!remainingWars.exists(w => w.onDifferentSides(Set(p.owner, p.controller)))) {
        p.controller = p.owner
      }
    }
  }

  def acceptPeaceTreaty(wa:WarAgreement, targets:Set[WarTarget], currentTurn:Int): Unit = {
    moveBackControlToOwnersAfterWar(wa)

    agreements = agreements.filterNot(_ == wa)
    val newAgreements = targets.flatMap(_.validTarget(wa, this)).flatMap {
      case tp:TakeProvince =>
        tp.province.owner = tp.demander
        tp.province.controller = tp.demander
        Nil
      case lc:LiberateCulture =>
        val newState = actions.generateNewState(lc.culture, lc.demander.rulingParty)
        lc.provinces.foreach { p =>
          p.owner = newState
          p.controller = newState
        }
        Nil
      case cs:CrackState =>
        val provinces = actions.regions.filter(_.owner == cs.giver)
        val max = provinces.maxBy(_.regionPopulation.populationCount)
        max.controller = max.owner
        provinces.filterNot(_ == max).foreach { p =>
          val newState = actions.generateNewState(p.culture, cs.demander.rulingParty)
          p.owner = newState
          p.controller = newState
        }
        Nil
      case tm:TakeMoney =>
        val money = tm.giver.budget.spendMoneyOnReparations(tm.amount)
        tm.demander.budget.receiveMoneyFromReparations(money)
        Nil
      case v:Vassalize =>
        List(new VassalAgreement(v.demander, v.giver, currentTurn))
    }

    newAgreements.foreach(this.addAgreement)
  }

}

object WorldDiplomacy {

  case class RelationshipBonus(from: State, to: State, bonus: Int, title: String)

}

sealed trait Claim {
  val state: State
}

object Claim {

  trait ProvinceClaim extends Claim {
    val state: State
    val province: Province
  }

  case class StrongProvinceClaim(state: State, province: Province) extends ProvinceClaim

  case class WeakProvinceClaim(state: State, province: Province, claimTurnEnd: Int) extends ProvinceClaim

  case class VassalizationClaim(state: State, possibleVassal:State, claimTurnEnd:Int) extends Claim
}

