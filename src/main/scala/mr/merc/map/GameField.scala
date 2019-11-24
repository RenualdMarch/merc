package mr.merc.map

import mr.merc.map.hex.TerrainHexField
import mr.merc.players.Player
import mr.merc.map.hex.TerrainHex
import mr.merc.unit.Soldier
import mr.merc.unit.Skirmisher

// TODO add allies support
class GameField(val hexField: TerrainHexField, val players: List[Player], val sides:Set[Set[Player]]) {
  def gridForSoldier(soldier: Soldier): UniversalGrid[TerrainHex] = {
    new UniversalGrid[TerrainHex] {
      private val mustStopHere = if (soldier.soldierType.attributes.contains(Skirmisher)) {
        Set.empty[TerrainHex]
      } else {
        players.filterNot(_ == soldier.owner).map(zoneOfControlFor).reduce(_ ++ _)
      }

      override def isBlocked(t: TerrainHex) = isBlockedFor(soldier, t)
      def neighbours(t: TerrainHex) = hexField.neighbours(t)
      override def price(from: TerrainHex, to: TerrainHex) = soldier.movementCostFunction(to)
      override def cellWhereMovementMustBeStopped(t: TerrainHex) = mustStopHere.contains(t)
      override def cellWhereItIsForbiddenToStop(t: TerrainHex) = t.soldier.map(_ != soldier).getOrElse(false)

      override def heuristic(from: TerrainHex, to: TerrainHex): Double = math.abs(from.x - to.x) + math.abs(from.y - to.y)
    }
  }

  private def areEnemies(p1:Player, p2:Player):Boolean = {
    sides.find(_.contains(p1)) != sides.find(_.contains(p2))
  }

  def isBlockedFor(soldier: Soldier, t: TerrainHex): Boolean = {
    t.soldier.exists(s => areEnemies(s.owner, soldier.owner))
  }

  def zoneOfControlFor(player: Player): Set[TerrainHex] = {
    // zone of control is every hex that is under enemy player or 
    // is neighbor to such hex
    hexField.hexes filter (h => hasPlayerOnHex(h, player) || hasPlayerOnNeighborHex(h, player)) toSet
  }

  // TODO add allies
  def zoneOfControlForEnemys(player: Player): Set[TerrainHex] = {
    players filterNot (_ == player) map (zoneOfControlFor) reduce (_ ++ _)
  }

  private def hasPlayerOnHex(hex: TerrainHex, player: Player): Boolean = {
    hex.soldier.exists(_.owner == player)
  }

  private def hasPlayerOnNeighborHex(hex: TerrainHex, player: Player): Boolean = {
    hexField.neighbours(hex).exists(hasPlayerOnHex(_, player))
  }
}