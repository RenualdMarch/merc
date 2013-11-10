package mr.merc.unit

import mr.merc.players.Player

class Soldier(val name:String, var soldierType:SoldierType, val player:Player) {
	private var currentHp = soldierType.hp
	var exp = 0
	var movePointsRemain = soldierType.movement
	def hp = currentHp
	var attackedThisTurn = false
	def movedThisTurn = movePointsRemain != soldierType.movement
	
	def damage(attack:Attack) {
	  currentHp -= soldierType.damageWithResistance(attack)
	  if (currentHp < 0) {
	    currentHp = 0
	  }
	}
	
	def resetMovePoints() {
	  movePointsRemain = soldierType.movement
	}
}