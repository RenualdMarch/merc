package mr.merc.diplomacy

import mr.merc.economics.Culture
import mr.merc.politics.{Province, State}

import mr.merc.economics.WorldConstants.Diplomacy._

abstract class RelationshipState(val relationshipBonus:Int) {

}

object RelationshipState {
  case object Neutral extends RelationshipState(NeutralRelationshipChange)
  case object War extends RelationshipState(WarRelationshipChange)
  case object Truce extends RelationshipState(TruceRelationshipChange)
  case object Alliance extends RelationshipState(AllianceRelationshipChange)
  case object Vassal extends RelationshipState(VassalRelationshipChange)
  case object Overlord extends RelationshipState(OverlordRelationshipChange)
}