package mr.merc.diplomacy

import mr.merc.diplomacy.DiplomaticAgreement.AgreementBreakingDetails
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement.WarTarget
import mr.merc.diplomacy.RelationshipEvent._
import mr.merc.diplomacy.WorldDiplomacy.RelationshipBonus
import mr.merc.economics.{BattleReport, Culture}
import mr.merc.politics.{Province, State}
import mr.merc.economics.WorldConstants.Diplomacy._
import mr.merc.local.Localization
import mr.merc.ui.world.IntFormatter

sealed abstract class DiplomaticAgreement(val signingTurn: Int, val duration: Option[Int]) {

  def sides: Set[State]

  def isPreviousAgreementBroken(agreement: DiplomaticAgreement, diplomacy: WorldDiplomacy): Option[AgreementBreakingDetails]

  def voluntaryBreakingAgreementEvent(breaker: State, currentTurn: Int): Set[RelationshipEvent]

  def isOver(currentTurn: Int): Boolean = duration.exists { d =>
    currentTurn < signingTurn + d
  }

  def relationshipBonus: List[RelationshipBonus]

  def durationEndAgreementEvent(currentTurn: Int): Set[RelationshipEvent]
}

object DiplomaticAgreement {

  case class AgreementBreakingDetails(breakers: Set[State], wasForcedToBreakAgreement: Boolean, agreementWasExtended: Boolean) {
    def isVoluntary: Boolean = !wasForcedToBreakAgreement && !agreementWasExtended

    def isInvoluntary: Boolean = !isVoluntary
  }

  object AgreementBreakingDetails {
    def forced(breakers: State*) = AgreementBreakingDetails(breakers.toSet, wasForcedToBreakAgreement = true, agreementWasExtended = false)

    def extended = AgreementBreakingDetails(Set(), wasForcedToBreakAgreement = false, agreementWasExtended = true)

    def voluntary(breakers: State*) = AgreementBreakingDetails(breakers.toSet, wasForcedToBreakAgreement = false, agreementWasExtended = false)
  }

  class AllianceAgreement(val first: State, val second: State, signingTurn: Int)
    extends DiplomaticAgreement(signingTurn, Some(DefaultAllianceDuration)) {

    override def sides: Set[State] = Set(first, second)

    override def durationEndAgreementEvent(currentTurn: Int): Set[RelationshipEvent] = {
      val List(first, second) = sides.toList
      Set(new WereTogetherInAlliance(first, second, currentTurn),
        new WereTogetherInAlliance(second, first, currentTurn))
    }

    override def isPreviousAgreementBroken(agreement: DiplomaticAgreement, diplomacy: WorldDiplomacy): Option[AgreementBreakingDetails] = {
      agreement match {
        case ag: AllianceAgreement if sides == ag.sides =>
          Some(AgreementBreakingDetails.extended)
        case _: AllianceAgreement => None
        case v: VassalAgreement if sides.contains(v.vassal) =>
          Some(AgreementBreakingDetails.forced(v.vassal))
        case _: VassalAgreement => None
        case _: TruceAgreement => None
        case wa: WarAgreement if wa.onDifferentSides(this.sides) =>
          val warIsValid = wa.removeSides(sides, diplomacy)
          if (warIsValid) None
          else Some(AgreementBreakingDetails.forced(first, second))
        case _: WarAgreement => None
      }
    }

    override def voluntaryBreakingAgreementEvent(breaker: State, currentTurn: Int): Set[RelationshipEvent] = {
      val victimSet = sides - breaker
      require(victimSet.size == 1)
      val victim = victimSet.head
      Set(new BrokeAlliance(victim, breaker, currentTurn))
    }

    def relationshipBonus: List[RelationshipBonus] = {
      List(RelationshipBonus(first, second, AllianceRelationshipChange, Localization("diplomacy.alliance", first.name, second.name)),
        RelationshipBonus(second, first, AllianceRelationshipChange, Localization("diplomacy.alliance", second.name, first.name)))
    }
  }

  class VassalAgreement(val overlord: State, val vassal: State, signingTurn: Int)
    extends DiplomaticAgreement(signingTurn, None) {

    override def sides: Set[State] = Set(overlord, vassal)

    override def isPreviousAgreementBroken(agreement: DiplomaticAgreement, diplomacy: WorldDiplomacy): Option[AgreementBreakingDetails] = {
      agreement match {
        case ag: AllianceAgreement if ag.sides.contains(vassal) => Some(AgreementBreakingDetails.forced(vassal))
        case _: AllianceAgreement => None
        case v: VassalAgreement if v.vassal == overlord => Some(AgreementBreakingDetails.forced(overlord))
        case v: VassalAgreement if v.overlord == vassal => Some(AgreementBreakingDetails.forced(vassal))
        case _: VassalAgreement => None
        case _: TruceAgreement => None
        case wa: WarAgreement if wa.onSameSide(this.sides) => None
        case wa: WarAgreement if wa.onDifferentSides(this.sides) =>
          val warIsValid = wa.removeSides(Set(vassal), diplomacy)
          if (warIsValid) {
            wa.addToTheSameSideAs(vassal, overlord)
            None
          } else Some(AgreementBreakingDetails.forced(vassal))
        case wa: WarAgreement if wa.sides.contains(overlord) =>
          wa.addToTheSameSideAs(vassal, overlord)
          None
        case wa: WarAgreement if wa.sides.contains(vassal) =>
          if (wa.removeSides(Set(vassal), diplomacy)) None
          else Some(AgreementBreakingDetails.forced(vassal))
        case _: WarAgreement => None

      }
    }

    override def voluntaryBreakingAgreementEvent(breaker: State, currentTurn: Int): Set[RelationshipEvent] = {
      if (breaker == overlord) {
        Set(new SetVassalFree(overlord, vassal, currentTurn))
      } else {
        Set(new VassalRevolting(overlord, vassal, currentTurn))
      }
    }

    override def durationEndAgreementEvent(currentTurn: Int): Set[RelationshipEvent] = Set()

    override def relationshipBonus: List[RelationshipBonus] = {
      List(RelationshipBonus(overlord, vassal, OverlordRelationshipChange, Localization("diplomacy.vassal", vassal.name, overlord.name)),
        RelationshipBonus(vassal, overlord, VassalRelationshipChange, Localization("diplomacy.vassal", vassal.name, overlord.name)))
    }
  }

  class TruceAgreement(val first: State, val second: State, signingTurn: Int)
    extends DiplomaticAgreement(signingTurn, Some(TruceDuration)) {

    override def sides: Set[State] = Set(first, second)

    override def isPreviousAgreementBroken(agreement: DiplomaticAgreement, diplomacy: WorldDiplomacy): Option[AgreementBreakingDetails] =
      agreement match {
        case t: TruceAgreement if t.sides == sides => Some(AgreementBreakingDetails.extended)
        case _: TruceAgreement => None
        case _: VassalAgreement => None
        case _: AllianceAgreement => None
        case _: WarAgreement => None
      }


    override def voluntaryBreakingAgreementEvent(breaker: State, currentTurn: Int): Set[RelationshipEvent] = {
      val victimSet = sides - breaker
      assert(victimSet.size == 1)
      Set(new BrokenTruce(victimSet.head, breaker, currentTurn))
    }

    override def durationEndAgreementEvent(currentTurn: Int): Set[RelationshipEvent] = Set()

    def relationshipBonus: List[RelationshipBonus] = {
      List(RelationshipBonus(first, second, TruceRelationshipChange, Localization("diplomacy.truce", first.name, second.name)),
        RelationshipBonus(second, first, TruceRelationshipChange, Localization("diplomacy.truce", first.name, second.name)))
    }

  }

  case class WarAgreement(var attackers: Set[State], var defenders: Set[State], warInitiator: State,
                          warVictim: State, startingTurn: Int, var targets: Set[WarTarget], fullWarName: String)
    extends DiplomaticAgreement(startingTurn, None) {

    private var _battles: List[BattleReport] = Nil

    def battles: List[BattleReport] = _battles

    def addBattle(report: BattleReport): Unit = {
      _battles ::= report
    }

    override def sides: Set[State] = attackers ++ defenders

    override def isPreviousAgreementBroken(agreement: DiplomaticAgreement, diplomacy: WorldDiplomacy): Option[AgreementBreakingDetails] = agreement match {
      case _: TruceAgreement => None
      case va: VassalAgreement if onDifferentSides(va.sides) => Some(AgreementBreakingDetails.forced(va.vassal, va.overlord))
      case _: VassalAgreement => None
      case aa: AllianceAgreement if onDifferentSides(aa.sides) => Some(AgreementBreakingDetails.forced(aa.first, aa.second))
      case _: AllianceAgreement => None
      case _: WarAgreement => None
    }

    def attackersLeader(diplomacy: WorldDiplomacy): State = diplomacy.getOverlord(warInitiator).getOrElse(warInitiator)

    def defendersLeader(diplomacy: WorldDiplomacy): State = diplomacy.getOverlord(warVictim).getOrElse(warVictim)

    def isLeader(state: State, diplomacy: WorldDiplomacy): Boolean =
      Set(attackersLeader(diplomacy), defendersLeader(diplomacy)).contains(state)

    def oppositeLeader(state: State, diplomacy: WorldDiplomacy): State = {
      if (attackers.contains(state)) defendersLeader(diplomacy)
      else if (defenders.contains(state)) attackersLeader(diplomacy)
      else sys.error(s"state $state doesn't belong to war $this")
    }

    override def voluntaryBreakingAgreementEvent(breaker: State, currentTurn: Int): Set[RelationshipEvent] = {
      if (attackers.contains(breaker)) {
        attackers -= breaker
        attackers.map(new SeparatePeace(breaker, _, currentTurn))
      } else if (defenders.contains(breaker)) {
        defenders -= breaker
        defenders.map(new SeparatePeace(breaker, _, currentTurn))
      } else {
        sys.error(s"Impossible case when $breaker escapes war [$attackers] vs [$defenders]")
      }
    }

    override def durationEndAgreementEvent(currentTurn: Int): Set[RelationshipEvent] = Set()

    override def relationshipBonus: List[RelationshipBonus] = attackers.flatMap { at =>
      defenders.flatMap { d =>
        Set(RelationshipBonus(at, d, WarRelationshipChange, Localization("diplomacy.atWar", at.name, d.name)),
          RelationshipBonus(d, at, WarRelationshipChange, Localization("diplomacy.atWar", d.name, at.name))
        )
      }
    } toList

    // boolean if the war is still valid
    def removeSides(removedSides: Set[State], diplomacy: WorldDiplomacy): Boolean = {
      attackers --= removedSides
      defenders --= removedSides
      targets = targets.filterNot(t => removedSides.contains(t.demander) || removedSides.contains(t.giver))
      !(attackers.isEmpty || defenders.isEmpty || targets.isEmpty ||
        removedSides.contains(attackersLeader(diplomacy)) || removedSides.contains(defendersLeader(diplomacy)))
    }

    def onDifferentSides(states: Set[State]): Boolean = {
      (attackers & states).nonEmpty && (defenders & states).nonEmpty
    }

    def containsSides(side1: Set[State], side2: Set[State]): Boolean = {
      (side1.subsetOf(attackers) && side2.subsetOf(defenders)) || (side2.subsetOf(attackers) && side1.subsetOf(defenders))
    }

    def onSameSide(states: Set[State]): Boolean = {
      states.subsetOf(attackers) || states.subsetOf(defenders)
    }

    def addToTheSameSideAs(whoToAdd: State, sameSideAs: State): Unit = {
      if (attackers.contains(sameSideAs)) {
        attackers += whoToAdd
      } else if (defenders.contains(sameSideAs)) {
        defenders += whoToAdd
      } else sys.error(s"Can't add $whoToAdd on same side as $sameSideAs when attackers [$attackers] and defenders [$defenders]")
    }

    def localizeWar: String = {
      Localization("diplomacy.war", attackers.map(_.name).mkString(","), defenders.map(_.name).mkString(","),
        WarAgreement.localizeTargetsList(targets.toList))
    }

    def localizeWarName: String = {
      Localization("diplomacy.warname", warInitiator.name, warVictim.name)
    }

    def sideByState(state: State): Set[State] =
      if (defenders.contains(state)) defenders
      else if (attackers.contains(state)) attackers
      else sys.error(s"Invalid side by state for state $state for war $this")

    def oppositeSideByState(state: State): Set[State] =
      if (attackers.contains(state)) defenders
      else if (defenders.contains(state)) attackers
      else sys.error(s"Invalid opposite side by state for state $state for war $this")

    def leaderByState(state: State, diplomacy: WorldDiplomacy): State = {
      if (defenders.contains(state))
        defendersLeader(diplomacy)
      else if (attackers.contains(state))
        attackersLeader(diplomacy)
      else sys.error(s"state $state doesn't belong to war $this")
    }

    override def clone(): WarAgreement = {
      val war = this.copy()
      this.battles.foreach(war.addBattle)
      war
    }
  }

  object WarAgreement {

    def localizeTargetsList(targets: List[WarTarget]): String = {
      if (targets.isEmpty) {
        Localization("diplomacy.proposePeace.whitePeace")
      } else {
        targets.map(t =>
          Localization("diplomacy.demands", t.demander.name) + " " + t.localizeTarget).mkString("\n")
      }
    }

    object WarTarget {

      def areWarTargetsConsistent(targets: Set[WarTarget]): Boolean = {
        !sameCultureLiberated(targets) &&
          !sameProvinceDemanded(targets) &&
          !sameMoneyDemanded(targets) &&
          !sameStateCracked(targets) &&
          !sameStateVassalized(targets)
      }

      private def sameCultureLiberated(targets: Set[WarTarget]): Boolean = {
        val lcSet = targets.collect {
          case lc: LiberateCulture => lc
        }

        val pairs = lcSet.map(lc => (lc.giver, lc.culture))

        lcSet.size != pairs.size
      }

      private def sameProvinceDemanded(targets: Set[WarTarget]): Boolean = {
        targets.collect {
          case t: TakeProvince => t
        }.groupBy(_.province).exists(_._2.size > 1)
      }

      private def sameMoneyDemanded(targets: Set[WarTarget]): Boolean = {
        sameGiverMoreThanOnce(
          targets.collect {
            case m: TakeMoney => m
          }
        )
      }

      private def sameStateCracked(targets: Set[WarTarget]): Boolean = {
        sameGiverMoreThanOnce(
          targets.collect {
            case c: CrackState => c
          }
        )
      }

      private def sameStateVassalized(targets: Set[WarTarget]): Boolean = {
        sameGiverMoreThanOnce(
          targets.collect {
            case v: Vassalize => v
          }
        )
      }

      private def sameGiverMoreThanOnce(targets: Set[WarTarget]): Boolean = {
        targets.groupBy(_.giver).exists(_._2.size > 1)
      }
    }

    sealed trait WarTarget {
      def demander: State

      def giver: State

      def validTarget(wa: WarAgreement, diplomacy: WorldDiplomacy): Option[WarTarget]

      def giverCanGiveInThisWar(wa: WarAgreement, diplomacy: WorldDiplomacy): Boolean = {
        val possibleDemanders = Set(wa.warVictim, wa.warInitiator) ++ diplomacy.getOverlord(wa.warInitiator) ++
          diplomacy.getOverlord(wa.warVictim) ++ diplomacy.getVassals(wa.warVictim) ++ diplomacy.getVassals(wa.warInitiator)
        val possibleGivers = Set(wa.warVictim, wa.warInitiator)
        possibleDemanders.contains(demander) && possibleGivers.contains(giver)
      }

      def localizeTarget: String
    }

    case class TakeProvince(demander: State, giver: State, val province: Province) extends WarTarget {
      override def validTarget(wa: WarAgreement, diplomacy: WorldDiplomacy): Option[WarTarget] = {
        if (giverCanGiveInThisWar(wa, diplomacy) && wa.onDifferentSides(Set(giver, demander)) &&
          giver == province.owner && wa.sides.contains(province.controller)) {
          Some(this)
        } else None
      }

      override def localizeTarget: String = Localization("diplomacy.warTarget.takeProvince", demander.initialName, province.name, giver.initialName)
    }

    case class LiberateCulture(demander: State, giver: State, val culture: Culture, val provinces: Set[Province]) extends WarTarget {
      override def validTarget(wa: WarAgreement, diplomacy: WorldDiplomacy): Option[WarTarget] = {
        if (giverCanGiveInThisWar(wa, diplomacy) && giver.primeCulture != culture) {
          val validProvinces = provinces.filter(_.owner == giver)
          if (validProvinces.nonEmpty) {
            Some(new LiberateCulture(demander, giver, culture, validProvinces))
          } else None
        } else None
      }

      override def localizeTarget: String = Localization("diplomacy.warTarget.liberateCulture", demander.initialName, culture.name, giver.initialName)

    }

    // every province became separate state
    case class CrackState(demander: State, giver: State, partiallyApplied: Boolean) extends WarTarget {
      override def validTarget(wa: WarAgreement, diplomacy: WorldDiplomacy): Option[WarTarget] = {
        if (giverCanGiveInThisWar(wa, diplomacy) && diplomacy.regions.count(_.owner == giver) > 1) {
          Some(this)
        } else None
      }

      override def localizeTarget: String = Localization("diplomacy.warTarget.crackState", demander.initialName, giver.initialName)
    }

    case class TakeMoney(demander: State, giver: State) extends WarTarget {
      override def validTarget(wa: WarAgreement, diplomacy: WorldDiplomacy): Option[WarTarget] = {
        if (giverCanGiveInThisWar(wa, diplomacy) && giver.budget.moneyReserve > 0) Some(TakeMoney(demander, giver))
        else None
      }

      override def localizeTarget: String = Localization("diplomacy.warTarget.takeMoney", demander.initialName, giver.initialName)
    }

    case class Vassalize(demander: State, giver: State) extends WarTarget {
      override def validTarget(wa: WarAgreement, diplomacy: WorldDiplomacy): Option[WarTarget] = {
        if (giverCanGiveInThisWar(wa, diplomacy)) Some(this) else None
      }

      override def localizeTarget: String = Localization("diplomacy.warTarget.vassalize", demander.initialName, giver.initialName)
    }

  }

}

