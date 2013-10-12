package mr.merc.map

import mr.merc.map.hex.TerrainHexField
import mr.merc.players.Player
import mr.merc.map.hex.TerrainHex
import mr.merc.unit.Soldier

// TODO add allies support
class GameField(val hexField:TerrainHexField, val players:List[Player]) {
	def gridForSoldier(soldier:Soldier):Grid[TerrainHex] = {
	  new Grid[TerrainHex] {
	    	override def isBlocked(t:TerrainHex) = isBlockedFor(soldier, t)
	    	def distance(from:TerrainHex, to:TerrainHex) = hexField.distance(from, to)
	    	def neighbours(t:TerrainHex) = hexField.neighbours(t)
	    	override def price(t:TerrainHex) = soldier.soldierType.moveCost(t.terrain)
	    	override def cellWhereMovementMustBeStopped(t:TerrainHex) = 
	    	  players.filterNot(_ == soldier.player).map(zoneOfControlFor).reduce(_ ++ _).contains(t)
	    	override def cellWhereItIsForbiddenToStop(t:TerrainHex) = t.soldier.map(_ != soldier).getOrElse(false) 
	  }
	}
	
	def isBlockedFor(soldier:Soldier, t:TerrainHex):Boolean = {
	  t.soldier.map(_.player != soldier.player).getOrElse(false)
	}
	
	
	def zoneOfControlFor(player:Player):Set[TerrainHex] = {
	  // zone of control is every hex that is under enemy player or 
	  // is neighbor to such hex
	  hexField.hexes filter (h => hasPlayerOnHex(h, player) || hasPlayerOnNeighborHex(h, player)) toSet
	}
	
	private def hasPlayerOnHex(hex:TerrainHex, player:Player):Boolean = {
	  hex.soldier.map(_.player == player).getOrElse(false)
	}
	
	private def hasPlayerOnNeighborHex(hex:TerrainHex, player:Player):Boolean = {
	  hexField.neighbours(hex).exists(hasPlayerOnHex(_, player))
	}
}