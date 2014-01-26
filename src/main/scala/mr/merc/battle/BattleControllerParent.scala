package mr.merc.battle

import javafx.stage.Window
import scalafx.beans.property.BooleanProperty
import mr.merc.unit.Soldier
import mr.merc.map.hex.TerrainHex
import mr.merc.unit.Attack

trait BattleControllerParent {
  def onMinimapChange(): Unit
  def disableEndTurn: BooleanProperty
  def showBattleOverDialog(result: BattleResult): Unit
  def showAttackSelectionDialog(attacker: Soldier, defender: Soldier, attackerHex: TerrainHex,
    defenderHex: TerrainHex): Option[Attack]
}