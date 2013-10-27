package mr.merc.battle.event

import mr.merc.unit.Soldier
import mr.merc.map.hex.TerrainHex
import mr.merc.unit.AttackResult
import mr.merc.players.Player

sealed trait BattleModelEvent {

}

sealed trait BattleModelEventResult {
  def buildBattleViewEvent:BattleViewEvent
}

trait BattleModelEventHandler {
  def handleEvent(event:BattleModelEvent):BattleModelEventResult
}

case class MovementModelEvent(soldier:Soldier, from:TerrainHex, to:TerrainHex) extends BattleModelEvent
case class AttackModelEvent(soldier:Soldier, from:TerrainHex, target:TerrainHex, attackNumber:Int) extends BattleModelEvent
case class EndMoveModelEvent() extends BattleModelEvent

case class AttackModelEventResult(attackerTerrainHex:TerrainHex, defenterTerrainHex:TerrainHex, result:List[AttackResult]) extends BattleModelEventResult {
  def buildBattleViewEvent = AttackBattleViewEvent(attackerTerrainHex, defenterTerrainHex, result)
}
case class MovementModelEventResult(soldier:Soldier, path:List[TerrainHex]) extends BattleModelEventResult {
  def buildBattleViewEvent = MoveBattleViewEvent(soldier, path)
}
case class EndMoveModelEventResult(nextPlayer:Player) extends BattleModelEventResult {
  def buildBattleViewEvent = EndMoveViewEvent(nextPlayer)
}
