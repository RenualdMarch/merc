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
    defenterTerrainHex:TerrainHex, attacker:Soldier, defender:Soldier, 
    result:List[AttackResult]) extends BattleViewEvent
case class MoveBattleViewEvent(soldier:Soldier, path:List[TerrainHex]) extends BattleViewEvent
case class EndMoveViewEvent(nextPlayer:Player) extends BattleViewEvent

case class ShowMovementOptions(hexes:Set[TerrainHex]) extends BattleViewEvent
case object HideMovementOptions extends BattleViewEvent
case class ShowArrow(src:TerrainHex, dest:TerrainHex) extends BattleViewEvent
case object HideArrow extends BattleViewEvent