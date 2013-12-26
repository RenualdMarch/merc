package mr.merc.unit

import scala.Option.option2Iterable
import scala.util.Random
import mr.merc.map.hex.TerrainHex
import mr.merc.map.objects.House
import mr.merc.map.terrain.Village
import mr.merc.map.objects.WoodenBridge
import mr.merc.map.terrain.Road
import mr.merc.map.terrain.Grass

object Attack {
  private val maxChance = 100
  def resolveAttack(chance: ChanceOfSuccess) = chance.chanceNumber >= Random.nextInt(maxChance)

  def battle(attackerHex: TerrainHex, defenderHex: TerrainHex, attackerSelection: Attack, defenderSelection: Option[Attack],
    f: ChanceOfSuccess => Boolean = resolveAttack): List[AttackResult] = {
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
      val attackerDefence = calculateSoldierDefence(attacker, attackerHex)
      val defenderDefence = calculateSoldierDefence(defender, defenderHex)
      val attackerStrikes = generateAttacks(true, attacker, defender, defenderDefence, attackerSelection, defenderSelection, f)
      val defenderStrikes = defenderSelection match {
        case Some(attack) => generateAttacks(false, defender, attacker, attackerDefence, attack, Some(attackerSelection), f)
        case None => Nil
      }

      if (defenderSelection.map(_.attributes.contains(Firststrike)).getOrElse(false) && !attackerSelection.attributes.contains(Firststrike)) {
        mergeAttacks(defenderStrikes, attackerStrikes)
      } else {
        mergeAttacks(attackerStrikes, defenderStrikes)
      }
    }

    val filteredAttacks = filterNotNeededAttacks(attacker, defender, resultAttacks.toList.flatten, attackerSelection, defenderSelection)

    // and here we are changing state
    filteredAttacks foreach (_.applyDamage())

    filteredAttacks
  }

  private def generateAttacks(attackerIsAttacking: Boolean, attacker: Soldier, defender: Soldier, defence: SoldierDefence, attackersAttack: Attack, defendersAttack: Option[Attack], f: ChanceOfSuccess => Boolean): List[AttackResult] = {
    val retVal = for (i <- 0 until attackersAttack.count) yield {
      val damage = Attack.possibleAttackersDamage(attackerIsAttacking, attacker, defender, attackersAttack, defendersAttack)
      val drained = if (attackersAttack.attributes.contains(Drain)) {
        damage / 2
      } else {
        0
      }
      val success = f(attackersAttack.chanceOfSuccess(defence))

      AttackResult(attackerIsAttacking, attacker, defender, attackersAttack, success, damage, drained)
    }

    retVal.toList
  }

  def calculateSoldierDefence(soldier: Soldier, hex: TerrainHex): SoldierDefence = {
    if (hex.mapObj == Some(House)) {
      SoldierDefence(soldier.soldierType.defence(Village))
    } else if (hex.mapObj == Some(WoodenBridge) || hex.terrain == Road) {
      SoldierDefence(soldier.soldierType.defence(Grass))
    } else {
      SoldierDefence(soldier.soldierType.defence(hex.terrain))
    }
  }

  private def mergeAttacks(attacker: List[AttackResult], defender: List[AttackResult], acc: List[AttackResult] = Nil): List[AttackResult] = {
    if (attacker.isEmpty) {
      acc ::: defender
    } else if (defender.isEmpty) {
      acc ::: attacker
    } else {
      mergeAttacks(attacker.tail, defender.tail, acc ::: List(attacker.head, defender.head))
    }
  }

  private def fixDrain(maxHp: Int, currentHp: Int, drain: Int): Int = {
    if (currentHp + drain > maxHp) {
      maxHp - currentHp
    } else {
      drain
    }
  }

  private def filterNotNeededAttacks(attacker: Soldier, defender: Soldier, attacks: List[AttackResult], attackerAttack: Attack, defenderAttack: Option[Attack]): List[AttackResult] = {
    var attackerState = attacker.hp
    var defenderState = defender.hp
    var attackerSlowed = attacker.state.contains(Slowed)
    var defenderSlowed = defender.state.contains(Slowed)

    attacks.flatMap(res => {
      if (attackerState <= 0 || defenderState <= 0) {
        None
      } else if (res.success) {
        if (res.attacker == attacker) {
          val possibleDamage = possibleAttackersDamage(true, attacker, defender, attackerAttack, defenderAttack)
          val damage = if (attackerSlowed) possibleDamage / 2 else possibleDamage
          defenderState -= damage
          if (attackerAttack.attributes.contains(Slow)) {
            defenderSlowed = true
          }

          if (defenderState < 0) {
            val actualDamage = damage + defenderState
            val drain = if (res.attackersAttack.attributes.contains(Drain)) {
              actualDamage / 2
            } else {
              0
            }
            val finalDrain = fixDrain(attacker.hp, attackerState, drain)
            attackerState += finalDrain

            Some(AttackResult(res.isAttackerAttackingThisRound, res.attacker, res.defender, res.attackersAttack, res.success, actualDamage, finalDrain))
          } else {
            val finalDrain = fixDrain(attacker.hp, attackerState, res.drained)
            attackerState += finalDrain
            Some(AttackResult(res.isAttackerAttackingThisRound, res.attacker, res.defender, res.attackersAttack, res.success, res.damage, finalDrain))
          }
        } else {
          val possibleDamage = possibleAttackersDamage(false, defender, attacker, defenderAttack.get, Some(attackerAttack))
          val damage = if (defenderSlowed) possibleDamage / 2 else possibleDamage
          attackerState -= damage
          if (defenderAttack.get.attributes.contains(Slow)) {
            attackerSlowed = true
          }

          if (attackerState < 0) {
            val actualDamage = damage + attackerState
            val drain = if (res.attackersAttack.attributes.contains(Drain)) {
              actualDamage / 2
            } else {
              0
            }

            val finalDrain = fixDrain(defender.hp, defenderState, drain)
            defenderState += finalDrain
            Some(AttackResult(res.isAttackerAttackingThisRound, res.attacker, res.defender, res.attackersAttack, res.success, actualDamage, finalDrain))
          } else {
            val finalDrain = fixDrain(defender.hp, defenderState, res.drained)
            defenderState += finalDrain
            Some(AttackResult(res.isAttackerAttackingThisRound, res.attacker, res.defender, res.attackersAttack, res.success, res.damage, finalDrain))
          }
        }
      } else {
        Some(res)
      }
    })
  }

  // when defender deals damage, first parameter is false, otherwise true
  def possibleAttackersDamage(actualAttackerAttacks: Boolean, attacker: Soldier, defender: Soldier, attackersAttack: Attack, defendersAttack: Option[Attack]): Int = {
    val resistance = if (actualAttackerAttacks && defender.soldierType.attributes.contains(Steadfast)) {
      val res = defender.soldierType.resistance(attackersAttack.attackType)
      if (res <= 0) {
        res
      } else if (res * 2 > 50) {
        50
      } else {
        res * 2
      }
    } else {
      defender.soldierType.resistance(attackersAttack.attackType)
    }

    val damageWithResistances = attackersAttack.damage * (100 - resistance) / 100
    val damage = if (actualAttackerAttacks && attackersAttack.attributes.contains(Charge) ||
      !actualAttackerAttacks && defendersAttack.map(_.attributes.contains(Charge)).getOrElse(false)) {
      damageWithResistances * 2
    } else {
      damageWithResistances
    }

    if (attacker.state.contains(Slowed)) damage / 2 else damage
  }

  def apply(imageName: String, damage: Int, count: Int, attackType: AttackType,
    ranged: Boolean) = new Attack(imageName, damage, count, attackType, ranged)

  def apply(imageName: String, damage: Int, count: Int, attackType: AttackType,
    ranged: Boolean, projectile: String) = new Attack(imageName, damage, count, attackType, ranged, projectile = Some(projectile))

  def selectBestAttackForDefender(attacker: Soldier, defender: Soldier, attackersAttack: Attack): Option[Attack] = {
    val ranged = attackersAttack.ranged
    val rangedAttacks = defender.soldierType.attacks.filter(_.ranged == ranged)
    if (rangedAttacks.isEmpty) {
      None
    } else if (rangedAttacks.size == 1) {
      Some(rangedAttacks(0))
    } else {
      val sorted = rangedAttacks.sortBy(ra => Attack.possibleAttackersDamage(false, defender, attacker, ra, Some(attackersAttack)) * ra.count)
      Some(sorted.last)
    }
  }
}

// TODO move image name and projectile name to view configuration
class Attack(val imageName: String, val damage: Int, val count: Int, val attackType: AttackType,
  val ranged: Boolean, val attributes: Set[AttackAttribute] = Set(), private val projectile: Option[String] = None) {
  def projectileName(success: Boolean) = success match {
    case true => projectile map (_ + "-succ")
    case false => projectile map (_ + "-fail")
  }

  def chanceOfSuccess(enemysDefence: SoldierDefence): ChanceOfSuccess = if (attributes.contains(Magical)) {
    ChanceOfSuccess(70)
  } else if (attributes.contains(Marksman) && enemysDefence.defence > 40) {
    ChanceOfSuccess(60)
  } else {
    ChanceOfSuccess(100 - enemysDefence.defence)
  }
}

case class ChanceOfSuccess(val chanceNumber: Int) extends AnyVal
case class SoldierDefence(val defence: Int) extends AnyVal