package mr.merc.battle.event

import mr.merc.unit.Soldier
import mr.merc.map.hex.TerrainHex
import mr.merc.unit.AttackResult
import mr.merc.players.Player

trait BattleViewEvent {

}

trait BattleViewEventHandler {
  def handleEvent(event:BattleViewEvent)
}

case class AttackBattleViewEvent(attackerTerrainHex:TerrainHex, defenterTerrainHex:TerrainHex, result:List[AttackResult])
case class MoveBattleViewEvent(soldier:Soldier, path:List[TerrainHex])
case class EndMoveViewEvent(nextPlayer:Player)