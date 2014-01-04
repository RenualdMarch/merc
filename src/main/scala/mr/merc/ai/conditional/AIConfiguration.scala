package mr.merc.ai.conditional

import mr.merc.ai.AttackResultPrediction
import mr.merc.ai.AttackResultPrediction

// TODO add comments or rename fields
case class AIConfiguration(highEnoughPercentOfEnemyDeath: Double,
  goodAttackCoef: Double, perfectAttackCofe: Double,
  maxDistanceForBestDefence: Int, rushCoef: Double, targets: Int) {

  def attackIsGood(prediction: AttackResultPrediction): Boolean = {
    attackCoefIsBigEnough(prediction, goodAttackCoef)
  }

  def attackIsPerfect(prediction: AttackResultPrediction): Boolean = {
    attackCoefIsBigEnough(prediction, perfectAttackCofe)
  }

  private def attackCoefIsBigEnough(prediction: AttackResultPrediction, coef: Double): Boolean = {
    if (prediction.expectedDamageToAttacker == 0) {
      true
    } else {
      val dif = prediction.expectedDamageToDefender / prediction.expectedDamageToAttacker
      dif >= coef
    }
  }
}