package mr.merc.ui.battle

import scalafx.scene.layout.VBox
import mr.merc.unit.Soldier
import scalafx.scene.canvas.Canvas
import mr.merc.ui.common.SoldierWrapper
import scalafx.scene.paint.Color
import scalafx.geometry.Pos._
import scalafx.beans.property.{DoubleProperty, IntegerProperty}
class SoldierViewControl(wrapper: SoldierWrapper, factor:Double) extends VBox {

  private val side = 72 * factor

  prefHeight = side
  prefWidth = side
  alignment = Center
  wrapper.image.onChange {
    refreshImage()
  }

  val canvas = new Canvas
  children = canvas
  canvas.width <== DoubleProperty(side)
  canvas.height <== DoubleProperty(side)

  refreshImage()

  def refreshImage() {
    val gc = canvas.graphicsContext2D
    gc.save()
    gc.fill = Color.Green
    gc.fillRect(0, 0, side, side)
    wrapper.image.value.drawImage(gc, 0, 0)
    gc.restore()
  }
}