package mr.merc.battle.event

import mr.merc.unit.Soldier
import mr.merc.map.hex.TerrainHex
import mr.merc.unit.AttackResult
import mr.merc.players.Player

sealed trait BattleViewEvent {

}

trait BattleViewEventHandler {
  def handleEvent(event:BattleViewEvent)
}

case class AttackBattleViewEvent(attackerTerrainHex:TerrainHex, 
    defenterTerrainHex:TerrainHex, result:List[AttackResult]) extends BattleViewEvent
case class MoveBattleViewEvent(soldier:Soldier, path:List[TerrainHex]) extends BattleViewEvent
case class EndMoveViewEvent(nextPlayer:Player) extends BattleViewEvent