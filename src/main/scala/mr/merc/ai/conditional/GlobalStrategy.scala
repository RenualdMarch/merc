package mr.merc.ai.conditional

import mr.merc.battle.BattleModel
import mr.merc.map.hex.TerrainHex
import mr.merc.ai.conditional.BattleModelHelper._
import mr.merc.log.Logging

class GlobalStrategy(conf: AIConfiguration) extends Logging {
  def decideGlobalCommand(model: BattleModel): GlobalCommand = {
    val friendsHp = model.friends.map(_._1.hp).sum
    val enemiesHp = model.enemies.map(_._1.hp).sum

    if (enemiesHp == 0) {
      debug("AI general orders Defend")
      DefendCommand
    } else {
      val targets = model.enemies.sortBy(_._1.hp).map(_._2).take(conf.targets)
      if (friendsHp / enemiesHp > conf.rushCoef) {
        debug("AI general orders Rush")
        RushCommand(targets)
      } else {
        debug("AI general orders Attack")
        AttackCommand(targets)
      }
    }
  }
}

sealed trait GlobalCommand
case class RushCommand(targets: List[TerrainHex]) extends GlobalCommand
case class AttackCommand(targets: List[TerrainHex]) extends GlobalCommand
case object DefendCommand extends GlobalCommand