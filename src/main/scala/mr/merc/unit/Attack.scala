package mr.merc.unit

import mr.merc.map.hex.TerrainHex
import scala.util.Random

object Attack {
  private val maxDefence = 100
  private def resolveAttack(defence:Int) = Random.nextInt(maxDefence) >= defence

  def battle(attackerHex:TerrainHex, defenderHex:TerrainHex, attackerSelection:Attack, defenderSelection:Option[Attack], 
      f:Int => Boolean = resolveAttack):List[AttackResult] = {
    require(attackerHex.soldier.isDefined)
    require(defenderHex.soldier.isDefined)
    
    val attacker = attackerHex.soldier.get
    val defender = defenderHex.soldier.get
    
    val attackerStrikes = generateAttacks(true, attacker, defender, defender.soldierType.defence(defenderHex.terrain), attackerSelection, defenderSelection, f)
    val defenderStrikes = defenderSelection match {
      case Some(attack) => generateAttacks(false, defender, attacker, attacker.soldierType.defence(attackerHex.terrain), attack, Some(attackerSelection), f)
      case None => Nil
    }
    
    val mergedAttacks = mergeAttacks(attackerStrikes, defenderStrikes)
    val filteredAttacks = filterNotNeededAttacks(attacker, defender, mergedAttacks, attackerSelection, defenderSelection)
    
    // and here we are changing state
    filteredAttacks foreach (_.applyDamage())
      
    filteredAttacks
  }
  
  private def generateAttacks(attackerIsAttacking:Boolean, attacker:Soldier, defender:Soldier, defence:Int, attackersAttack:Attack, defendersAttack:Option[Attack], f:Int => Boolean):List[AttackResult] = {
    val retVal = for (i <- 0 until attackersAttack.count) yield {
      AttackResult(attackerIsAttacking, attacker, defender, attackersAttack, defendersAttack, f(defence))
    }
    
    retVal.toList
  }
  
  private def mergeAttacks(attacker:List[AttackResult], defender:List[AttackResult], acc:List[AttackResult] = Nil):List[AttackResult] = {
    if (attacker.isEmpty) {
      acc ::: defender
    } else if (defender.isEmpty) {
      acc ::: attacker
    } else {
      mergeAttacks(attacker.tail, defender.tail, acc ::: List(attacker.head, defender.head))
    }
  }
  
  private def filterNotNeededAttacks(attacker:Soldier, defender:Soldier, attacks:List[AttackResult], attackerAttack:Attack, defenderAttack:Option[Attack]):List[AttackResult] = {
    var attackerState = attacker.hp
    var defenderState = defender.hp
    
    attacks.takeWhile(res => {
      if (attackerState <= 0 || defenderState <= 0) {
        false
      } else if (res.success) {
        if (res.attacker == attacker) {
          defenderState -= possibleAttackersDamage(true, attacker, defender, attackerAttack, defenderAttack)
        } else {
          attackerState -= possibleAttackersDamage(false, defender, attacker, defenderAttack.get, Some(attackerAttack))
        }
        true
      } else {
        true
      }
    })    

  }
  
      // when defender deals damage, first parameter is false, otherwise true
  def possibleAttackersDamage(actualAttackerAttacks:Boolean, attacker:Soldier, defender:Soldier, attackersAttack:Attack, defendersAttack:Option[Attack]):Int = {
	attackersAttack.damage * (100 + defender.soldierType.resistance(attackersAttack.attackType)) / 100
  }
  
  def apply(imageName:String, damage:Int, count:Int, attackType:AttackType, 
    ranged:Boolean) = new Attack(imageName, damage, count, attackType, ranged)
  
  def apply(imageName:String, damage:Int, count:Int, attackType:AttackType, 
    ranged:Boolean, projectile:String) = new Attack(imageName, damage, count, attackType, ranged, projectile = Some(projectile))
}

// TODO move image name and projectile name to view configuration
class Attack(val imageName:String, val damage:Int, val count:Int, val attackType:AttackType, 
    val ranged:Boolean, val attributes:Set[AttackAttribute] = Set(), private val projectile:Option[String] = None) {
	def projectileName(success:Boolean) = success match {
	  case true => projectile map (_ + "-succ")
	  case false => projectile map (_ + "-fail")
	}
}