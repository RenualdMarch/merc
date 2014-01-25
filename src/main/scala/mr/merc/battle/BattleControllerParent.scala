package mr.merc.battle

import javafx.stage.Window
import scalafx.beans.property.BooleanProperty

trait BattleControllerParent {
  def window: Window
  def onMinimapChange(): Unit
  val disableEndTurn: BooleanProperty
}