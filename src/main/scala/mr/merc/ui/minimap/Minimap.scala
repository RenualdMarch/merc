package mr.merc.ui.minimap

import scalafx.scene.canvas.Canvas
import mr.merc.map.hex.TerrainHexField
import scalafx.scene.paint.Color
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import javafx.beans.property.SimpleDoubleProperty
import scala.beans.BeanProperty
import scalafx.scene.layout.VBox

class Minimap(field: TerrainHexField) extends VBox {
  private val canvas = new Canvas()
  content = canvas
  canvas.width <== width
  canvas.height <== height
  canvas.width.onChange(redraw())
  canvas.height.onChange(redraw())

  def redraw() {
    draw()
  }

  private def draw() {
    val w = width.value
    val h = height.value
    if (w == 0 || h == 0) {
      return ;
    }

    val gc = canvas.graphicsContext2D
    gc.save()
    gc.fill = Color.BLACK
    gc.fillRect(0, 0, w, h)

    val horizontalSide = w / field.width
    val verticalSide = h / field.height

    field.hexes.foreach { hex =>
      val x = hex.x * horizontalSide
      val offset = if (hex.x % 2 == 0) 0 else verticalSide / 2
      val y = hex.y * verticalSide + offset
      gc.fill = color(hex)
      gc.fillRect(x, y, horizontalSide, verticalSide)
    }

    gc.restore()
  }

  private def color(hex: TerrainHex): Color = {
    if (hex.soldier.nonEmpty) {
      return hex.soldier.get.player.color
    }

    hex.terrain match {
      case Water => Color.BLUE
      case Forest => Color.GREEN
      case Grass => Color.LIGHTGREEN
      case Sand => Color.YELLOW
      case Swamp => Color.BROWN
      case Hill => Color.LIGHTGRAY
      case Mountain => Color.GRAY
      case Road => Color.ORANGE
      case a: Any => throw new IllegalArgumentException(s"Unknown color $a")
    }
  }
}
