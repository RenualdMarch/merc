package mr.merc.battle.event

import mr.merc.unit.Soldier
import mr.merc.map.hex.TerrainHex
import mr.merc.unit.AttackResult

sealed trait BattleModelEvent {

}

sealed trait BattleModelEventResult {
  
}

trait BattleModelEventHandler {
  def handleEvent(event:BattleModelEvent):BattleModelEventResult
}

case class MovementModelEvent(soldier:Soldier, from:TerrainHex, to:TerrainHex) extends BattleModelEvent
case class AttackModelEvent(soldier:Soldier, from:TerrainHex, target:TerrainHex, attackNumber:Int) extends BattleModelEvent
case class EndMoveModelEvent() extends BattleModelEvent

case class AttackModelEventResult(attackerTerrainHex:TerrainHex, defenterTerrainHex:TerrainHex, attacker:Soldier, defender:Soldier, result:List[AttackResult]) extends BattleModelEventResult
case class MovementModelEventResult(soldier:Soldier, path:List[TerrainHex]) extends BattleModelEventResult
case class EndMoveModelEventResult() extends BattleModelEventResult
