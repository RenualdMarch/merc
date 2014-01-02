package mr.merc.ai

import mr.merc.battle.BattleModel
import mr.merc.battle.event.EndMoveModelEvent

class DummyAI extends BattleAI {
  override def nextTurn(model: BattleModel) = EndMoveModelEvent
}