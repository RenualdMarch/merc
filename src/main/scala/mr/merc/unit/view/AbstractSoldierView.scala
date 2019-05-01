package mr.merc.unit.view

import scalafx.scene.paint.Color
import mr.merc.view.Sprite
import scalafx.scene.canvas.GraphicsContext

abstract class AbstractSoldierView(viewInfo: SoldierTypeViewInfo, factor: Double) extends Sprite[SoldierViewState](viewInfo.images, StandState, factor) {
  viewInfo.eagerLoad()

  centered = Some(factor * 72 toInt)

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

  def drawOvalUnderSoldier(gc: GraphicsContext, xOffset: Int, yOffset: Int, color: Color) {
    gc.save()
    gc.fill = color
    gc.globalAlpha = 0.2
    gc.fillOval(x + xOffset + 12 * factor, y + yOffset + 44 * factor, 48 * factor, 24 * factor)
    gc.globalAlpha = 1
    gc.stroke = color
    gc.strokeOval(x + xOffset + 12 * factor, y + yOffset + 44 * factor, 48 * factor, 24 * factor)
    gc.restore()
  }
}