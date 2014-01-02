package mr.merc.ai.conditional

import mr.merc.battle.BattleModel

class GlobalStrategy(conf: AIConfiguration) {
  def decideGlobalCommand(model: BattleModel): GlobalCommand = {
    ???
  }
}

sealed trait GlobalCommand
case object RushCommand extends GlobalCommand
case object AttackCommand extends GlobalCommand
case object DefendCommand extends GlobalCommand