package mr.merc.ai.conditional

import mr.merc.unit.Soldier
import mr.merc.battle.BattleModel
import mr.merc.battle.event.BattleModelEvent
import mr.merc.ai.conditional.BattleModelHelper._
import mr.merc.map.hex.TerrainHex
import mr.merc.unit.Attack
import mr.merc.ai.AttackResultPrediction
import mr.merc.ai.AttackResultPredictor
import mr.merc.ai.AttackSelectionHelper
import mr.merc.battle.event.MovementModelEvent
import mr.merc.battle.event.MovementModelEvent
import mr.merc.battle.event.MovementModelEvent
import mr.merc.battle.event.AttackModelEvent

case class AIAgent(soldier: Soldier, hex: TerrainHex, conf: AIConfiguration) {
  def makeMove(command: GlobalCommand, model: BattleModel): Option[List[BattleModelEvent]] = {
    val targetOpt = bestTarget(model)

    // if can kill someone or attack makes sense, should do it
    if (targetOpt.map(_.prediction.defenderDies >= conf.highEnoughPercentOfEnemyDeath).getOrElse(false)
      || targetOpt.map(t => conf.attackIsPerfect(t.prediction)).getOrElse(false)) {
      return Some(targetToEvent(targetOpt.get))
    }

    // TODO add healing
    command match {
      case RushCommand(targets) => {
        // if it is possible to attack, should attack
        if (targetOpt.isDefined) {
          Some(targetToEvent(targetOpt.get))
        } else {
          Some(List(moveToClosestEnemy(model, targets)))
        }
      }
      case AttackCommand(targets) => {
        if (targetOpt.map(t => conf.attackIsGood(t.prediction)).getOrElse(false)) {
          Some(targetToEvent(targetOpt.get))
        } else {
          Some(List(moveToClosestEnemy(model, targets)))
        }
      }
      case DefendCommand => {
        // TODO think what should be done when there are only few soldiers left?
        None
      }
    }
  }

  private def targetToEvent(target: TargetInfo): List[BattleModelEvent] = {
    if (target.hexFromWhichAttack != hex) {
      List(MovementModelEvent(soldier, hex, target.hexFromWhichAttack),
        AttackModelEvent(soldier, target.hexFromWhichAttack, target.hexWhichAttack, target.attack.index))
    } else {
      List(AttackModelEvent(soldier, target.hexFromWhichAttack, target.hexWhichAttack, target.attack.index))
    }
  }

  private def moveToClosestEnemy(model: BattleModel, enemies: List[TerrainHex]): MovementModelEvent = {
    moveCloserToEnemy(selectClosestEnemy(enemies), model)
  }

  // TODO
  // By now it uses simple distance, in case of lakes it will move through them
  private[conditional] def selectClosestEnemy(enemies: List[TerrainHex]): TerrainHex = {
    enemies.sortBy(hex.distance).head
  }

  private[conditional] def moveCloserToEnemy(enemy: TerrainHex, model: BattleModel): MovementModelEvent = {
    val possibleMoves = model.possibleMoves(soldier, hex).toList
    val minDistance = enemy.distance(possibleMoves.minBy(enemy.distance))
    val acceptableDistance = minDistance + conf.maxDistanceForBestDefence
    val hexesOnAcceptableDistance = possibleMoves.filter(h => enemy.distance(h) <= acceptableDistance)
    val hexWithMaxDefence = hexesOnAcceptableDistance.maxBy(h => Attack.calculateSoldierDefence(soldier, h).defence)
    val maxDefence = Attack.calculateSoldierDefence(soldier, hexWithMaxDefence)
    val targetHex = hexesOnAcceptableDistance.sortBy(enemy.distance).dropWhile(h => Attack.calculateSoldierDefence(soldier, h) != maxDefence).head

    MovementModelEvent(soldier, hex, targetHex)
  }

  private[conditional] def bestTarget(model: BattleModel): Option[TargetInfo] = {
    val reachableHexes = model.possibleMoves(soldier, hex) ++ Set(hex)
    val reachableHexesAndNeigs = reachableHexes.flatMap(model.map.hexField.neighbours(_))
    val reachableEnemies = reachableHexesAndNeigs.filter(_.soldier.isDefined).filter(_.soldier.get.owner.isEnemy(soldier.owner))
    val enemyAndAttackPossitions = reachableEnemies.map(h => (h, model.map.hexField.neighboursSet(h) & reachableHexes))
    val targets = enemyAndAttackPossitions map {
      case (enemyHex, attackPositions) =>
        val bestHexForAttack = attackPositions.toList.sortBy(h => Attack.calculateSoldierDefence(soldier, h).defence).last
        val attacker = soldier
        val defender = enemyHex.soldier.get
        val attackersAttackIndex = AttackSelectionHelper.selectBestAttack(attacker, defender, bestHexForAttack, enemyHex)
        val attackersAttack = attacker.soldierType.attacks(attackersAttackIndex)
        val prediction = AttackResultPredictor.predictResult(soldier, enemyHex.soldier.get, bestHexForAttack, enemyHex, attackersAttack, Attack.selectBestAttackForDefender(soldier, enemyHex.soldier.get, attackersAttack))
        require(bestHexForAttack != enemyHex, "Same hexes, attack is invalid")
        TargetInfo(bestHexForAttack, enemyHex, attackersAttack, prediction)
    }

    if (targets.nonEmpty) {
      Some(targets.toList.sorted.last)
    } else {
      None
    }
  }

  private[conditional] case class TargetInfo(hexFromWhichAttack: TerrainHex, hexWhichAttack: TerrainHex, attack: Attack, prediction: AttackResultPrediction) extends Ordered[TargetInfo] {
    def compare(that: TargetInfo): Int = {
      val perc = conf.highEnoughPercentOfEnemyDeath
      if (this.prediction.defenderDies >= perc &&
        that.prediction.defenderDies < perc) {
        1
      } else if (this.prediction.defenderDies < perc &&
        that.prediction.defenderDies >= perc) {
        -1
      } else if (this.prediction.defenderDies >= perc &&
        that.prediction.defenderDies >= perc) {
        this.hexWhichAttack.soldier.get.soldierType.cost - that.hexWhichAttack.soldier.get.soldierType.cost
      } else {
        val thisExpectedDamageToAttacker = if (this.prediction.expectedDamageToAttacker == 0) 0.01
        else this.prediction.expectedDamageToAttacker
        val thisAttackerDamageRatio = this.prediction.expectedDamageToDefender / thisExpectedDamageToAttacker

        val thatExpectedDamageToAttacker = if (that.prediction.expectedDamageToAttacker == 0) 0.01
        else that.prediction.expectedDamageToAttacker
        val thatAttackerDamageRatio = that.prediction.expectedDamageToDefender / thatExpectedDamageToAttacker
        val result = thisAttackerDamageRatio - thatAttackerDamageRatio
        if (result > 0) {
          1
        } else if (result < 0) {
          -1
        } else {
          0
        }
      }
    }
  }
}