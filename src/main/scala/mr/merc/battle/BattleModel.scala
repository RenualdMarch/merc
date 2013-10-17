package mr.merc.battle

import mr.merc.map.GameField
import mr.merc.battle.event._
import mr.merc.unit.Soldier
import mr.merc.map.hex.TerrainHex
import mr.merc.map.pathfind.AStarPathFinder
import mr.merc.map.terrain.TerrainType
import mr.merc.unit.Attack

class BattleModel(val map:GameField) extends BattleModelEventHandler {
	
    def handleEvent(event:BattleModelEvent):BattleModelEventResult = {
	  event match {
	    case MovementModelEvent(soldier, from, to) => handleMovementEvent(soldier, from, to)
	    case AttackModelEvent(soldier, from, target, attackNumber) => handleAttackEvent(soldier, from, target, attackNumber)
	    case EndMoveModelEvent() => ???
	  }	  
	}
    
    private [battle] def handleMovementEvent(soldier:Soldier, from:TerrainHex, to:TerrainHex):MovementModelEventResult = {
      require(validateMovementEvent(soldier, from, to))
      require(to.soldier == None)
      val path = AStarPathFinder.findPath(map.gridForSoldier(soldier), from, to)
      val movePrice = pathPrice(soldier.soldierType.moveCost, path.get)
      soldier.movePointsRemain -= movePrice
      from.soldier = None
      to.soldier = Some(soldier)
      
      new MovementModelEventResult(soldier, path.get)
    }
    
    private [battle] def handleAttackEvent(soldier:Soldier, from:TerrainHex, target:TerrainHex, attackNumber:Int):AttackModelEventResult = {
      require(validateAttackEvent(soldier, from, target, attackNumber))
      val defender = target.soldier.get
      
      val attackerAttack = soldier.soldierType.attacks(attackNumber)
      val defenderAttack = selectBestAttackForDefender(soldier, defender, attackerAttack)
      soldier.attackedThisTurn = true
      val result = Attack.battle(from, target, attackerAttack, defenderAttack)
      new AttackModelEventResult(from, target, soldier, defender, result)
    }
    
    private [battle] def selectBestAttackForDefender(attacker:Soldier, defender:Soldier, attackersAttack:Attack):Option[Attack] = {
      val ranged = attackersAttack.ranged
      val rangedAttacks = defender.soldierType.attacks.filter(_.ranged == ranged)
      if (rangedAttacks.isEmpty) {
        None
      } else if (rangedAttacks.size == 1) {
        Some(rangedAttacks(0))
      } else {
        val sorted = rangedAttacks.sortBy(ra => attacker.soldierType.damageWithResistance(ra) * ra.count)
        Some(sorted.last)
      }
     }
    
    
    def validateMovementEvent(soldier:Soldier, from:TerrainHex, to:TerrainHex):Boolean = {
      if (soldier.attackedThisTurn) {
        return false
      }
      
      if (soldier.movedThisTurn && map.zoneOfControlForEnemys(soldier.player).contains(from)) {
        return false
      }
      
      val pathOpt = AStarPathFinder.findPath(map.gridForSoldier(soldier), from, to)
      pathOpt match {
        case Some(path) => {
          val price = pathPrice(soldier.soldierType.moveCost, path)
          price <= soldier.movePointsRemain
        }
        case None => false
      }      
    }
    
    // TODO allies
    def validateAttackEvent(soldier:Soldier, from:TerrainHex, target:TerrainHex, attackNumber:Int):Boolean = {
      if (soldier.soldierType.attacks.size <= attackNumber) {
        return false
      }
      
      if (!map.hexField.neighbours(from).contains(target)) {
        return false
      }
      
      target.soldier match {
        case Some(enemy) => soldier.player != enemy.player
        case None => false
      }
    }
    
    private def pathPrice(cost:Map[TerrainType, Int], path:List[TerrainHex]):Int = {
      path.tail.map(t => cost(t.terrain)).sum
    }
}