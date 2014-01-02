package mr.merc.ai

import mr.merc.battle.BattleModel
import mr.merc.battle.event.BattleModelEvent

trait BattleAI {
  def nextTurn(model: BattleModel): BattleModelEvent
}