package mr.merc.ai.conditional

import mr.merc.ai.BattleAI
import mr.merc.battle.event.BattleModelEvent
import mr.merc.battle.BattleModel
import mr.merc.ai.conditional.BattleModelHelper._
import mr.merc.battle.event.EndMoveModelEvent
import mr.merc.ai.AIQueueAdapter

// TODO test me
class ConditionalAI(config: AIConfiguration) extends BattleAI with AIQueueAdapter {
  def nextTurns(model: BattleModel): List[BattleModelEvent] = {
    val command = new GlobalStrategy(config).decideGlobalCommand(model)
    val soldiersAndHexes = model.friends.sortBy(_._1.movePointsRemain)
    val stream = soldiersAndHexes.toStream.map {
      case (s, h) =>
        val agent = new AIAgent(s, h, config)
        agent.makeMove(command, model)
    }

    stream.dropWhile(_.isEmpty)
    if (stream.isEmpty) {
      List(EndMoveModelEvent)
    } else {
      stream.head.get
    }
  }
}