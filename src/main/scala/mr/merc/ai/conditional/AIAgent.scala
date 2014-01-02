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

case class AIAgent(soldier: Soldier, hex: TerrainHex, conf: AIConfiguration) {
  def makeMove(command: GlobalCommand, model: BattleModel): Option[BattleModelEvent] = {
    ???
  }

  private[conditional] def bestTarget(model: BattleModel): Option[TargetInfo] = {
    val reachableHexes = model.possibleMoves(soldier, hex) ++ Set(hex)
    val reachableHexesAndNeigs = reachableHexes.flatMap(model.map.hexField.neighbours(_))
    val reachableEnemies = reachableHexesAndNeigs.filter(_.soldier.isDefined).filter(_.soldier.get.player != soldier.player)
    val enemyAndAttackPossitions = reachableEnemies.map(h => (h, model.map.hexField.neighbours(h) & reachableHexes))
    val targets = enemyAndAttackPossitions map {
      case (enemyHex, attackPositions) =>
        val bestHexForAttack = attackPositions.toList.sortBy(h => Attack.calculateSoldierDefence(soldier, h).defence).last
        val attacker = soldier
        val defender = enemyHex.soldier.get
        val attackersAttackIndex = AttackSelectionHelper.selectBestAttack(attacker, defender, bestHexForAttack, enemyHex)
        val attackersAttack = attacker.soldierType.attacks(attackersAttackIndex)
        val prediction = AttackResultPredictor.predictResult(soldier, enemyHex.soldier.get, bestHexForAttack, enemyHex, attackersAttack, Attack.selectBestAttackForDefender(soldier, enemyHex.soldier.get, attackersAttack))
        TargetInfo(bestHexForAttack, enemyHex, prediction)
    }

    if (targets.nonEmpty) {
      Some(targets.toList.sorted.last)
    } else {
      None
    }
  }

  private[conditional] case class TargetInfo(hexFromWhichAttack: TerrainHex, hexWhichAttack: TerrainHex, prediction: AttackResultPrediction) extends Ordered[TargetInfo] {
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