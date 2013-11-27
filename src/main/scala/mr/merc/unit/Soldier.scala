package mr.merc.unit

import mr.merc.players.Player

class Soldier(val name:String, var soldierType:SoldierType, val player:Player) {
	private var _hp = soldierType.hp
	private var _state = Set[SoldierState]()
	
	def hp = _hp
	def hp_=(newHp:Int) {
	  _hp = newHp
	  if (_hp > soldierType.hp) {
	    _hp = soldierType.hp
	  } else if (_hp < 0) {
	    _hp = 0
	  }
	}
	
	var exp = 0
	var movePointsRemain = soldierType.movement
	var attackedThisTurn = false
	def movedThisTurn = movePointsRemain != soldierType.movement
	
	def endMove() {
	  movePointsRemain = soldierType.movement
	  attackedThisTurn = false
	  if (state.contains(Poisoned)) {
	    hp -= 8
	    if (hp <= 0) hp = 1
	  }
	  
	  if (state.contains(Slowed)) {
	    removeState(Slowed)
	  }
	}
	
	def addState(state:SoldierState) {
	  _state += state
	}
	
	def removeState(state:SoldierState) {
	  _state -= state
	}
	
	def state = _state
}