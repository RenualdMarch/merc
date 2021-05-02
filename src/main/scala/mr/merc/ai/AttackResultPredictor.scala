package mr.merc.ai

import mr.merc.map.hex.TerrainHex
import mr.merc.unit.Soldier
import mr.merc.unit.Attack
import mr.merc.unit.SoldierState
import mr.merc.unit.AttackAttribute._

object AttackResultPredictor {

  def predictResult(attacker: Soldier, defender: Soldier, attackerHex: TerrainHex, defenderHex: TerrainHex, attackersAttack: Attack, defendersAttack: Option[Attack]): AttackResultPrediction = {
    val variations = calculateVariations(attacker, defender, attackerHex, defenderHex, attackersAttack, defendersAttack)
    val attackerDiesChance = variations.filter(_.damageToAttacker == attacker.hp).map(_.chance).sum
    val defenderDiesChance = variations.filter(_.damageToDefender == defender.hp).map(_.chance).sum
    val avgDamageToDefender = variations.map(v => v.chance * v.damageToDefender).sum
    val avgDamageToAttacker = variations.map(v => v.chance * v.damageToAttacker).sum

    AttackResultPrediction(attackerDiesChance, defenderDiesChance, avgDamageToAttacker, avgDamageToDefender)
  }

  def calculateVariations(attacker: Soldier, defender: Soldier, attackerHex: TerrainHex, defenderHex: TerrainHex, attackersAttack: Attack, defendersAttack: Option[Attack]): List[Variation] = {
    val generatedAttacks = generateAttacks(attackersAttack, defendersAttack)

    val rootNode = PredictionTreeNode(1d, AttackInfo(attacker, defender, attackerHex, defenderHex, attackersAttack, defendersAttack), attacker.hp, defender.hp, generatedAttacks)

    def findAllLeaves(node: PredictionTreeNode): List[PredictionTreeNode] = {
      if (node.isLeaf) {
        List(node)
      } else {
        findAllLeaves(node.onAttackSuccess) ::: findAllLeaves(node.onAttackFailure)
      }
    }

    val leaves = findAllLeaves(rootNode)
    leaves map (_.createVariation)
  }

  private def generateAttacks(attackersAttack: Attack, defendersAttack: Option[Attack]): List[AttackTry] = {
    val berserk = attackersAttack.attributes.contains(Berserk) || defendersAttack.map(_.attributes.contains(Berserk)).getOrElse(false)
    val repeats = if (berserk) 5 else 1
    val result = for (i <- 0 until repeats) yield {
      attackCycle(attackersAttack, defendersAttack)
    }

    result.flatten.toList
  }

  private def attackCycle(attackersAttack: Attack, defendersAttack: Option[Attack]): List[AttackTry] = {
    val attacker = List.fill(attackersAttack.count)(AttackTry(true))
    val defender = defendersAttack.map { a =>
      List.fill(a.count)(AttackTry(false))
    }.getOrElse(Nil)

    def merge[T](first: List[T], second: List[T], acc: List[T] = Nil): List[T] = {
      if (first.nonEmpty && second.nonEmpty) {
        merge(first.tail, second.tail, first.head :: second.head :: acc)
      } else if (first.isEmpty) {
        acc ::: second
      } else if (second.isEmpty) {
        acc ::: first
      } else {
        sys.error("Impossible condition")
      }
    }

    if (defendersAttack.map(_.attributes.contains(Firststrike)).getOrElse(false)) {
      merge(defender, attacker)
    } else {
      merge(attacker, defender)
    }
  }
}

case class PredictionTreeNode(chance: Double, info: AttackInfo, attackersHp: Int, defendersHp: Int, remaining: List[AttackTry]) {
  def isLeaf = remaining.isEmpty || attackersHp == 0 || defendersHp == 0
  def createVariation: Variation = {
    require(isLeaf, "Cann't create variation - not a leaf")
    val damageToAttacker = info.attacker.hp - attackersHp
    val damageToDefender = info.defender.hp - defendersHp
    Variation(chance, damageToAttacker, damageToDefender)
  }

  def chanceOfSuccess: Double = {
    if (remaining.head.isAttackersAttacking) {
      val defence = Attack.calculateSoldierDefence(info.defender, info.defenderHex)
      val chance = info.attackersAttack.chanceOfSuccess(defence)
      chance.chanceNumber / 100d
    } else {
      val defence = Attack.calculateSoldierDefence(info.attacker, info.attackerHex)
      val chance = info.defendersAttack.get.chanceOfSuccess(defence)
      chance.chanceNumber / 100d
    }
  }

  def possibleDamage: Int = if (remaining.head.isAttackersAttacking) {
    Attack.possibleAttackersDamage(true, info.attacker, info.defender, info.attackersAttack, info.defendersAttack)
  } else {
    Attack.possibleAttackersDamage(false, info.defender, info.attacker, info.defendersAttack.get, Some(info.attackersAttack))
  }

  def drained: Int = if (remaining.head.isAttackersAttacking) {
    if (info.attackersAttack.attributes.contains(Drain)) {
      possibleDamage / 2
    } else {
      0
    }
  } else {
    if (info.defendersAttack.get.attributes.contains(Drain)) {
      possibleDamage / 2
    } else {
      0
    }
  }

  def normalizeHp(hp: Int, soldier: Soldier): Int = {
    if (hp < 0) 0 else Math.min(hp, soldier.soldierType.hp)
  }

  def onAttackSuccess: PredictionTreeNode = {
    require(!isLeaf, "is leaf")
    if (remaining.head.isAttackersAttacking) {
      val defHp = normalizeHp(defendersHp - possibleDamage, info.defender)
      val attHp = normalizeHp(attackersHp + drained, info.attacker)
      PredictionTreeNode(chance * chanceOfSuccess, info, attHp, defHp, remaining.tail)
    } else {
      val defHp = normalizeHp(defendersHp + drained, info.defender)
      val attHp = normalizeHp(attackersHp - possibleDamage, info.attacker)
      PredictionTreeNode(chance * chanceOfSuccess, info, attHp, defHp, remaining.tail)
    }
  }

  def onAttackFailure: PredictionTreeNode = {
    require(!isLeaf, "is leaf")
    val chanceOfFailure = 1 - chanceOfSuccess
    PredictionTreeNode(chance * chanceOfFailure, info, attackersHp, defendersHp, remaining.tail)
  }
}

case class AttackInfo(attacker: Soldier, defender: Soldier, attackerHex: TerrainHex, defenderHex: TerrainHex, attackersAttack: Attack, defendersAttack: Option[Attack])
case class AttackTry(isAttackersAttacking: Boolean)
case class Variation(chance: Double, damageToAttacker: Int, damageToDefender: Int)
case class AttackResultPrediction(attackerDies: Double, defenderDies: Double, expectedDamageToAttacker: Double, expectedDamageToDefender: Double)