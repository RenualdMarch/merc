package mr.merc.battle

import javafx.stage.Window

trait BattleControllerParent {
  def window: Window
  def onMinimapChange(): Unit
}