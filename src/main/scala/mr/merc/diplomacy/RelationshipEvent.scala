package mr.merc.diplomacy

import mr.merc.diplomacy.WorldDiplomacy.RelationshipBonus
import mr.merc.politics.State
import mr.merc.economics.WorldConstants.Diplomacy._
import mr.merc.local.Localization

sealed abstract class RelationshipEvent(val fromState: State, val toState: State, val eventTurn: Int, val duration: Int, val relationshipChange: Int) {

  def relationshipsChange(currentTurn: Int): RelationshipBonus = {
    val passedDuration = currentTurn - eventTurn

    val change = if (passedDuration > duration) 0
    else (relationshipChange * (duration - passedDuration).toDouble / duration).toInt

    RelationshipBonus(fromState, toState, change, localizeEvent)
  }

  def isDurationEnded(currentTurn: Int): Boolean = duration + eventTurn - currentTurn <= 0

  def localizeEvent: String
}

object RelationshipEvent {

  class BrokeAlliance(loyal: State, betrayer: State, eventTurn: Int)
    extends RelationshipEvent(loyal, betrayer, eventTurn, AllianceBetrayalDuration, AllianceBetrayalRelationshipsChange) {

    override def localizeEvent: String = Localization("diplomacy.brokeAlliance", betrayer.name, loyal.name)
  }

  class BrokeFriendshipTreaty(loyal: State, betrayer: State, eventTurn: Int)
    extends RelationshipEvent(loyal, betrayer, eventTurn, FriendshipBetrayalDuration, FriendshipBetrayalRelationshipsChange) {

    override def localizeEvent: String = Localization("diplomacy.brokeFriendship", betrayer.name, loyal.name)
  }

  class DeclinedAlliance(proposingState: State, rejectingState: State, eventTurn: Int)
    extends RelationshipEvent(proposingState, rejectingState, eventTurn, AllianceRejectionDuration, AllianceRejectionRelationshipChange) {

    override def localizeEvent: String = Localization("diplomacy.rejectedAlliance", rejectingState.name, proposingState.name)
  }

  class RejectedFriendship(proposingState: State, rejectingState: State, eventTurn: Int)
    extends RelationshipEvent(proposingState, rejectingState, eventTurn, FriendshipRejectionDuration, FriendshipRejectionRelationshipChange) {

    override def localizeEvent: String = Localization("diplomacy.rejectedFriendship", rejectingState.name, proposingState.name)
  }

  class DeclinedVassalization(proposingState: State, rejectingState: State, eventTurn: Int)
    extends RelationshipEvent(proposingState, rejectingState, eventTurn, VassalRejectionDuration, VassalRejectionRelationshipChange) {

    override def localizeEvent: String = Localization("diplomacy.rejectedVassalization", rejectingState.name, proposingState.name)
  }

  class DeclinedOverlordship(proposingState: State, rejectingState: State, eventTurn: Int)
    extends RelationshipEvent(proposingState, rejectingState, eventTurn, OverlordshipRejectionDuration, OverlordshipRejectionRelationshipChange) {

    override def localizeEvent: String = Localization("diplomacy.rejectedOverlordship", rejectingState.name, proposingState.name)
  }


  class SetVassalFree(prevLord: State, freeVassal: State, eventTurn: Int)
    extends RelationshipEvent(freeVassal, prevLord, eventTurn, SetVassalFreeDuration, SetVassalFreeRelationshipChange) {

    override def localizeEvent: String = Localization("diplomacy.setVassalFree", prevLord.name, freeVassal.name)
  }

  class VassalRevolting(prevLord: State, freeVassal: State, eventTurn: Int)
    extends RelationshipEvent(prevLord, freeVassal, eventTurn, VassalRevoltingDuration, VassalRevoltingRelationshipChange) {

    override def localizeEvent: String = Localization("diplomacy.revoltingVassal", freeVassal.name, prevLord.name)
  }

  class BrokenTruce(victim: State, breaker: State, eventTurn: Int)
    extends RelationshipEvent(victim, breaker, eventTurn, BrokenTruceDuration, BrokenTruceRelationshipChange) {

    override def localizeEvent: String = Localization("diplomacy.brokenTruce", breaker.name, victim.name)
  }

  class WereInWar(first: State, second: State, eventTurn: Int)
    extends RelationshipEvent(first, second, eventTurn, WereInWarDuration, WereInWarRelationshipChange) {

    override def localizeEvent: String = Localization("diplomacy.wereInWar", first.name, second.name)
  }

  class SeparatePeace(escaping: State, remaining: State, eventTurn: Int)
    extends RelationshipEvent(remaining, escaping, eventTurn, SeparatePeaceDuration, SeparatePeaceRelationshipChange) {

    override def localizeEvent: String = Localization("diplomacy.separatePeace", escaping.name, remaining.name)
  }

  class AcceptedJoinWar(warInitiator: State, warJoiner: State, eventTurn: Int)
    extends RelationshipEvent(warInitiator, warJoiner, eventTurn, AllianceHonoredDuration, AllianceHonoredRelationshipChange) {

    override def localizeEvent: String = Localization("diplomacy.joinedWar", warJoiner.name, warInitiator.name)
  }

}