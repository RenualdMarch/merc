package mr.merc.ai

import mr.merc.map.hex.TerrainHex
import mr.merc.unit.Soldier
import mr.merc.unit.Attack

object AttackSelectionHelper {
  def selectBestAttack(attacker: Soldier, defender: Soldier, attackerHex: TerrainHex, defenderHex: TerrainHex): Int = {

    // arguments are attacks with index
    def selectBestAttack(attacks: List[(Attack, Int)]): Int = {
      attacks.map {
        case (a, n) =>
          val defenderAttack = Attack.selectBestAttackForDefender(attacker, defender, a)
          val damage = Attack.possibleAttackersDamage(true, attacker, defender, a, defenderAttack) * a.count
          val chance = a.chanceOfSuccess(Attack.calculateSoldierDefence(defender, defenderHex))
          (damage * chance.chanceNumber, n)
      }.sortBy(-_._1).head._2
    }

    val attacksWithoutAnswer = attacker.soldierType.attacks.zipWithIndex.flatMap {
      case (a, i) =>
        val defenderAttack = Attack.selectBestAttackForDefender(attacker, defender, a)
        defenderAttack match {
          case Some(_) => None
          case None => Some(a, i)
        }
    }

    if (attacksWithoutAnswer.nonEmpty) {
      selectBestAttack(attacksWithoutAnswer)
    } else {
      selectBestAttack(attacker.soldierType.attacks.zipWithIndex)
    }

  }

}