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
    
    val rounds = if (attackerSelection.attributes.contains(Berserk) || 
        defenderSelection.map(_.attributes.contains(Berserk)).getOrElse(false)) {
      30
    } else {
      1
    }
    
    val resultAttacks = for (i <- 0 until rounds) yield {
      val attackerStrikes = generateAttacks(true, attacker, defender, defender.soldierType.defence(defenderHex.terrain), attackerSelection, defenderSelection, f)
      val defenderStrikes = defenderSelection match {
        case Some(attack) => generateAttacks(false, defender, attacker, attacker.soldierType.defence(attackerHex.terrain), attack, Some(attackerSelection), f)
        case None => Nil
      }
      
      mergeAttacks(attackerStrikes, defenderStrikes)
    }
    
    val filteredAttacks = filterNotNeededAttacks(attacker, defender, resultAttacks.toList.flatten, attackerSelection, defenderSelection)
    
    // and here we are changing state
    filteredAttacks foreach (_.applyDamage())
      
    filteredAttacks
  }
  
  private def generateAttacks(attackerIsAttacking:Boolean, attacker:Soldier, defender:Soldier, defence:Int, attackersAttack:Attack, defendersAttack:Option[Attack], f:Int => Boolean):List[AttackResult] = {
  
    val retVal = for (i <- 0 until attackersAttack.count) yield {
      val damage = Attack.possibleAttackersDamage(attackerIsAttacking, attacker, defender, attackersAttack, defendersAttack)
      val drained = if (attackersAttack.attributes.contains(Drain)) {
        damage / 2
      } else {
        0
      }
      AttackResult(attackerIsAttacking, attacker, defender, attackersAttack, defendersAttack, f(defence), damage, drained)
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
  
  private def fixDrain(maxHp:Int, currentHp:Int, drain:Int):Int = {
    if (currentHp + drain > maxHp) {
      maxHp - currentHp
    } else {
      drain
    }
  }
  
  private def filterNotNeededAttacks(attacker:Soldier, defender:Soldier, attacks:List[AttackResult], attackerAttack:Attack, defenderAttack:Option[Attack]):List[AttackResult] = {
    var attackerState = attacker.hp
    var defenderState = defender.hp
    
    attacks.flatMap(res => {
      if (attackerState <= 0 || defenderState <= 0) {
        None
      } else if (res.success) {
        if (res.attacker == attacker) {
          val damage = possibleAttackersDamage(true, attacker, defender, attackerAttack, defenderAttack)
          defenderState -= damage
          if (defenderState < 0) {
            val actualDamage = damage + defenderState
            val drain = if (res.attackersAttack.attributes.contains(Drain)) {
              actualDamage / 2
            } else {
              0
            }
            val finalDrain = fixDrain(attacker.hp, attackerState, drain)
            attackerState += finalDrain

            Some(AttackResult(res.isAttackerAttackingThisRound, res.attacker, res.defender, res.attackersAttack, res.defendersAttack, res.success, actualDamage, finalDrain))
          } else {
            val finalDrain = fixDrain(attacker.hp, attackerState, res.drained)
            attackerState += finalDrain
            Some(AttackResult(res.isAttackerAttackingThisRound, res.attacker, res.defender, res.attackersAttack, res.defendersAttack, res.success, res.damage, finalDrain))
          }          
        } else {
          val damage = possibleAttackersDamage(false, defender, attacker, defenderAttack.get, Some(attackerAttack))
          attackerState -= damage
          if (attackerState < 0) {
            val actualDamage = damage + attackerState
            val drain = if (res.attackersAttack.attributes.contains(Drain)) {
              actualDamage / 2
            } else {
              0
            }
            
            val finalDrain = fixDrain(defender.hp, defenderState, drain)
            defenderState += finalDrain
            Some(AttackResult(res.isAttackerAttackingThisRound, res.attacker, res.defender, res.attackersAttack, res.defendersAttack, res.success, actualDamage, finalDrain))
          } else {
            val finalDrain = fixDrain(defender.hp, defenderState, res.drained)
            defenderState += finalDrain
            Some(AttackResult(res.isAttackerAttackingThisRound, res.attacker, res.defender, res.attackersAttack, res.defendersAttack, res.success, res.damage, finalDrain))
          }          
        }
      } else {
        Some(res)
      }
    })    

  }
  
      // when defender deals damage, first parameter is false, otherwise true
  def possibleAttackersDamage(actualAttackerAttacks:Boolean, attacker:Soldier, defender:Soldier, attackersAttack:Attack, defendersAttack:Option[Attack]):Int = {
	val damageWithResistances = attackersAttack.damage * (100 + defender.soldierType.resistance(attackersAttack.attackType)) / 100
	if (actualAttackerAttacks && attackersAttack.attributes.contains(Charge) || 
	    !actualAttackerAttacks && defendersAttack.map(_.attributes.contains(Charge)).getOrElse(false)) {
	  damageWithResistances * 2
	} else {
	  damageWithResistances
	}
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