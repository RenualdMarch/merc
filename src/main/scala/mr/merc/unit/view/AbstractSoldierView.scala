package mr.merc.unit.view

import scalafx.scene.paint.Color
import mr.merc.view.Sprite

abstract class AbstractSoldierView(viewInfo: SoldierTypeViewInfo) extends Sprite[SoldierViewState](viewInfo.images, StandState) {
  viewInfo.eagerLoad()

  override def updateTime(delta: Int): Int = {
    val result = super.updateTime(delta)

    if (result > 0) {
      // TODO optimize it in case of 1-frame 
      markAsDirty()
    }

    if (result > 0 && index == 0) {
      if (state == DeathState) {
        state = NoState
      } else if (state == IdleState) {
        state = StandState
      }
    }
    result
  }
}