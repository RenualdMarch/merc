package mr.merc.unit

import mr.merc.players.Player
import mr.merc.map.hex.TerrainHexField

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
	def needsHealing = hp != soldierType.hp
	def endMove() {
	  movePointsRemain = soldierType.movement
	  attackedThisTurn = false
	  
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
	
	// TODO split into methods
	def beforeTurnActions(field:TerrainHexField, x:Int, y:Int):List[BeforeTurnAction] = {
	  val attributes = soldierType.attributes
	  val result = collection.mutable.ArrayBuffer[BeforeTurnAction]()
	  
	  val neighbours = field.neighbours(x, y).flatMap(_.soldier).filter(_.player == this.player)
	  if (attributes.contains(Cures)) {
	    val allies = neighbours.filter(_.state.contains(Poisoned))
	    result ++= allies.map(t => CureSoldier(this, t))
	  }
	  
	  val heal = if (attributes.contains(Heals4)) Some(Heals4) 
			  else if (attributes.contains(Heals8)) Some(Heals8)
			  else None
	  
      heal match {
		case Some(healing) => {
		  val whoNeedsHealing = neighbours.filter(_.needsHealing)
		  val actions = healing match {
		    case Heals4 => whoNeedsHealing.map(Heal4Soldier(this, _))
		    case Heals8 => whoNeedsHealing.map(Heal8Soldier(this, _))
		  }
		  result ++= actions
		}
		case None => // do nothing
	  }
	  
	  if (attributes.contains(Regenerates) && 
	      (needsHealing || state.contains(Poisoned))) {
	    result += Regeneration(this)
	  }
	  
	  if (state.contains(Poisoned)) {
	    result += PoisoningDamage(this)
	  }
	  
	  result toList
	}
}