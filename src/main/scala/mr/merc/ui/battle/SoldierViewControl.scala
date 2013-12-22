package mr.merc.ui.battle

import scalafx.scene.layout.VBox
import mr.merc.unit.Soldier
import scalafx.scene.canvas.Canvas
import mr.merc.ui.common.SoldierWrapper
import scalafx.scene.paint.Color
import scalafx.geometry.Pos._
import scalafx.beans.property.IntegerProperty
class SoldierViewControl(wrapper: SoldierWrapper) extends VBox {

  private val side = 72

  prefHeight = side
  prefWidth = side
  alignment = CENTER
  wrapper.image.onChange {
    refreshImage()
  }

  val canvas = new Canvas
  content = canvas
  canvas.width <== IntegerProperty(side)
  canvas.height <== IntegerProperty(side)

  refreshImage()

  def refreshImage() {
    val gc = canvas.graphicsContext2D
    gc.save()
    gc.fill = Color.BLACK
    gc.fillRect(0, 0, side, side)
    wrapper.image.value.drawImage(gc, 0, 0)
    gc.restore()
  }
}