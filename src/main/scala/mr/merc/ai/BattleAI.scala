package mr.merc.ai

import mr.merc.battle.BattleModel
import mr.merc.battle.event.BattleModelEvent
import mr.merc.ai.conditional.AIConfiguration
import mr.merc.ai.conditional.ConditionalAI

object BattleAI {

  // default implementation
  def apply(): BattleAI = {
    val conf = AIConfiguration(0.5, 0.5, 0.2, 2, 3, 2)
    new ConditionalAI(conf)
  }
}

trait BattleAI {
  def nextTurn(model: BattleModel): BattleModelEvent
}