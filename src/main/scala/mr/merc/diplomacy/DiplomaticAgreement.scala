package mr.merc.diplomacy

import mr.merc.diplomacy.DiplomaticAgreement.AgreementBreakingDetails
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement.WarTarget
import mr.merc.diplomacy.RelationshipEvent._
import mr.merc.diplomacy.WorldDiplomacy.RelationshipBonus
import mr.merc.economics.Culture
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

  class WarAgreement(var attackers: Set[State], var defenders: Set[State], val warInitiator: State,
                     val warVictim: State, startingTurn: Int, var targets: Set[WarTarget])
    extends DiplomaticAgreement(startingTurn, None) {

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
  }

  object WarAgreement {

    def localizeTargetsList(targets:List[WarTarget]):String = targets.map(t =>
      Localization("diplomacy.demands", t.demander.name) + " " + t.localizeTarget).mkString("\n")


    sealed abstract class WarTarget(val demander: State, val giver: State) {
      def validTarget(wa: WarAgreement, diplomacy: WorldDiplomacy): Option[WarTarget]

      def giverCanGiveInThisWar(wa: WarAgreement, diplomacy: WorldDiplomacy): Boolean = {
        val possibleDemanders = Set(wa.warVictim, wa.warInitiator) ++ diplomacy.getOverlord(wa.warInitiator) ++
          diplomacy.getOverlord(wa.warVictim) ++ diplomacy.getVassals(wa.warVictim) ++ diplomacy.getVassals(wa.warInitiator)
        val possibleGivers = Set(wa.warVictim, wa.warInitiator)
        possibleDemanders.contains(demander) && possibleGivers.contains(giver)
      }

      def localizeTarget: String
    }

    class TakeProvince(demander: State, giver: State, val province: Province) extends WarTarget(demander, giver) {
      override def validTarget(wa: WarAgreement, diplomacy: WorldDiplomacy): Option[WarTarget] = {
        if (giverCanGiveInThisWar(wa, diplomacy) && wa.onDifferentSides(Set(giver, demander)) &&
          giver == province.owner && wa.sides.contains(province.controller)) {
          Some(this)
        } else None
      }

      override def localizeTarget: String = Localization("diplomacy.warTarget.takeProvince", province.name, giver.name)
    }

    class LiberateCulture(demander: State, giver: State, val culture: Culture, val provinces: Set[Province]) extends WarTarget(demander, giver) {
      override def validTarget(wa: WarAgreement, diplomacy: WorldDiplomacy): Option[WarTarget] = {
        if (giverCanGiveInThisWar(wa, diplomacy) && giver.primeCulture != culture) {
          val validProvinces = provinces.filter(_.owner == giver)
          if (validProvinces.nonEmpty) {
            Some(new LiberateCulture(demander, giver, culture, validProvinces))
          } else None
        } else None
      }

      override def localizeTarget: String = Localization("diplomacy.warTarget.liberateCulture", culture.name, giver.name)

    }

    // every province became separate state
    class CrackState(demander: State, giver: State) extends WarTarget(demander, giver) {
      override def validTarget(wa: WarAgreement, diplomacy: WorldDiplomacy): Option[WarTarget] = {
        if (giverCanGiveInThisWar(wa, diplomacy) && diplomacy.regions.count(_.owner == giver) > 1) {
          Some(this)
        } else None
      }

      override def localizeTarget: String = Localization("diplomacy.warTarget.crackState", giver.name)
    }

    class TakeMoney(demander: State, giver: State, val amount: Double) extends WarTarget(demander, giver) {
      override def validTarget(wa: WarAgreement, diplomacy: WorldDiplomacy): Option[WarTarget] = {
        if (giverCanGiveInThisWar(wa, diplomacy) && giver.budget.moneyReserve > 0) Some(new TakeMoney(demander, giver, giver.budget.moneyReserve))
        else None
      }

      override def localizeTarget: String = Localization("diplomacy.warTarget.takeMoney", IntFormatter().format(amount), giver.name)
    }

    class Vassalize(demander: State, giver: State) extends WarTarget(demander, giver) {
      override def validTarget(wa: WarAgreement, diplomacy: WorldDiplomacy): Option[WarTarget] = {
        if (giverCanGiveInThisWar(wa, diplomacy)) Some(this) else None
      }

      override def localizeTarget: String = Localization("diplomacy.warTarget.vassalize", giver.name)
    }

  }

}

