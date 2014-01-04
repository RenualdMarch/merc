package mr.merc.ai

import mr.merc.battle.event.BattleModelEvent
import mr.merc.battle.BattleModel

trait AIQueueAdapter extends BattleAI {
  private val queue = collection.mutable.Queue[BattleModelEvent]()
  final def nextTurn(model: BattleModel): BattleModelEvent = {
    if (queue.isEmpty) {
      nextTurns(model) foreach (e => queue.enqueue(e))
    }

    queue.dequeue()
  }

  def nextTurns(model: BattleModel): List[BattleModelEvent]
}