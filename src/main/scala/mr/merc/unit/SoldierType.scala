package mr.merc.unit

import mr.merc.map.terrain._

case class SoldierType(name: String, cost: Int, hp: Int, movement: Int, exp: Int, level: Int,
                       attacks: List[Attack], moveCost: Map[TerrainKind, Int], defence: Map[DefenceType, Int],
                       resistance: Map[AttackType, Int], attributes: Set[SoldierTypeAttribute] = Set(), viewName: String) {
}

sealed trait DefenceType

object DefenceType {

  case object WaterDefence extends DefenceType
  case object ForestDefence extends DefenceType
  case object SwampDefence extends DefenceType
  case object HillDefence extends DefenceType
  case object MountainDefence extends DefenceType
  case object SandDefence extends DefenceType
  case object GrassDefence extends DefenceType
  case object BuildingDefence extends DefenceType
  case object SnowDefence extends DefenceType
  case object IceDefence extends DefenceType
}

