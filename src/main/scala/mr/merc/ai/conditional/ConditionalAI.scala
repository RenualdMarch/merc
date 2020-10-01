package mr.merc.ai.conditional

import mr.merc.ai.BattleAI
import mr.merc.battle.event.BattleModelEvent
import mr.merc.battle.BattleModel
import mr.merc.ai.conditional.BattleModelHelper._
import mr.merc.battle.event.EndMoveModelEvent
import mr.merc.ai.AIQueueAdapter
import mr.merc.unit.Soldier
import mr.merc.log.Logging

// TODO test me
class ConditionalAI(config: AIConfiguration) extends BattleAI with AIQueueAdapter with Logging {
  var soldierStates: Map[Soldier, SoldierAiState] = Map()

  def nextTurns(model: BattleModel): List[BattleModelEvent] = {
    if (soldierStates.isEmpty) { // means that this is first call during this turn
      soldierStates ++= model.currentSoldiers.map(s => (s._1, HaventMoved)).toMap
    }

    val command = new GlobalStrategy(config).decideGlobalCommand(model)
    val soldiersAndHexes = model.currentSoldiers.filter(s => soldierStates(s._1) == HaventMoved) sortBy (_._1.movePointsRemain)

    val stream = soldiersAndHexes.toStream.map {
      case (s, h) =>
        val agent = AIAgent(s, h, config)
        soldierStates += s -> Moved
        agent.makeMove(command, model)
    }

    stream.dropWhile(_.isEmpty)
    if (stream.isEmpty) {
      soldierStates = Map()
      List(EndMoveModelEvent)
    } else {
      debug(s"AI decided to do ${stream.head}")
      stream.head.getOrElse(List(EndMoveModelEvent))
    }
  }

  sealed trait SoldierAiState
  case object HaventMoved extends SoldierAiState
  case object Moved extends SoldierAiState

}