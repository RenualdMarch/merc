package mr.merc.players

import scalafx.scene.paint.Color
import mr.merc.ai.BattleAI

case class Player(name: String, color: Color = Color.Red, ai: Option[BattleAI] = None) {
  var allies: List[Player] = Nil

  def isSamePlayer(player: Player) = player == this
  def isFriend(player: Player) = allies.contains(player) || isSamePlayer(player)
  def isEnemy(player: Player) = !isFriend(player)
}