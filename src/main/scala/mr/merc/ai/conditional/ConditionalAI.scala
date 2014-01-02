package mr.merc.ai.conditional

import mr.merc.ai.BattleAI
import mr.merc.battle.event.BattleModelEvent
import mr.merc.battle.BattleModel

class ConditionalAI(config: AIConfiguration) extends BattleAI {
  def nextTurn(model: BattleModel): BattleModelEvent = ???
}