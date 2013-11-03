package mr.merc.battle

import mr.merc.map.GameField
import mr.merc.battle.event._
import mr.merc.unit.Soldier
import mr.merc.map.hex.TerrainHex
import mr.merc.map.pathfind.AStarPathFinder
import mr.merc.map.terrain.TerrainType
import mr.merc.unit.Attack
import mr.merc.map.pathfind.MercPossibleMovesFinder

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
    
     def hexBySoldier(soldierOpt:Option[Soldier]):Option[TerrainHex] = soldierOpt map hexBySoldier
     
     def hexBySoldier(soldier:Soldier) = map.hexField.hexes.find(h => h.soldier == Some(soldier)).get
    
     def validateMovementEvent(soldier:Soldier, from:TerrainHex, to:TerrainHex, validatePath:Boolean = true):Boolean = {
      if (soldier.attackedThisTurn) {
        return false
      }
      
      if (soldier.movedThisTurn && map.zoneOfControlForEnemys(soldier.player).contains(from)) {
        return false
      }
      
      if (validatePath) {
        val pathOpt = AStarPathFinder.findPath(map.gridForSoldier(soldier), from, to)
        pathOpt match {
          case Some(path) => {
            val price = pathPrice(soldier.soldierType.moveCost, path)
            price <= soldier.movePointsRemain
          }
          case None => false
        }
      } else {
        true
      }
    }
     
    def possibleMoves(soldier:Soldier, currentHex:TerrainHex):Set[TerrainHex] = {
      val possible = MercPossibleMovesFinder.findPossibleMoves(map.gridForSoldier(soldier), currentHex, soldier.movePointsRemain, soldier.movedThisTurn)
      possible filter (validateMovementEvent(soldier, currentHex, _, false))
    }
    
    def possibleAttacksWhenThereAreNoMoves(soldier:Soldier, currentHex:TerrainHex):Set[TerrainHex] = {
      val neigbours = map.hexField.neighbours(currentHex)
      val enemiesNear = neigbours.filter(_.soldier.map(_.player != soldier.player).getOrElse(false))
      if (enemiesNear.isEmpty) {
        return Set()
      }
      
      if (soldier.movedThisTurn) {
        enemiesNear
      } else {
        Set()
      }
     }
    
    // TODO add test
    def validateMovementAndAttack(soldier:Soldier, start:TerrainHex, destination:TerrainHex, underAttack:TerrainHex, attackNumber:Int):Boolean = {
      if (start != destination) {
        validateMovementEvent(soldier, start, destination) && validateAttackEvent(soldier, destination, underAttack, attackNumber)
      } else {
        validateAttackEvent(soldier, destination, underAttack, attackNumber)
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