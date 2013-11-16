package mr.merc.unit

import mr.merc.players.Player

class Soldier(val name:String, var soldierType:SoldierType, val player:Player) {
	var hp = soldierType.hp
	var exp = 0
	var movePointsRemain = soldierType.movement
	var attackedThisTurn = false
	def movedThisTurn = movePointsRemain != soldierType.movement
	
	
	def resetMovePoints() {
	  movePointsRemain = soldierType.movement
	}
}