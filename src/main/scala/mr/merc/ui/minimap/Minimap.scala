package mr.merc.ui.minimap

import scalafx.scene.canvas.Canvas
import mr.merc.map.hex.TerrainHexField
import scalafx.scene.paint.Color
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import javafx.beans.property.SimpleDoubleProperty
import scala.beans.BeanProperty
import scalafx.scene.layout.VBox
import scalafx.scene.control.ScrollPane
import mr.merc.map.view.MapView
import scalafx.beans.binding.Bindings._

class Minimap(field: TerrainHexField, pane: ScrollPane) extends VBox {
  private val canvas = new Canvas()
  private val mapView = new MapView(field)
  private var updateOnPaneChange = true

  // proportion is pane side / this side, expected to be more than 1
  private val mapToMinimapWidthProportion = when(this.width.isEqualTo(0)).choose(0).otherwise(pane.width / this.width)
  private val mapToMinimapHeightProportion = when(this.height.isEqualTo(0)).choose(0).otherwise(pane.height / this.height)
  private val visiblePartWidth = pane.width / mapView.pixelWidth
  private val visiblePartHeight = pane.height / mapView.pixelHeight
  private val rectWidth = visiblePartWidth * this.width
  private val rectHeight = visiblePartHeight * this.height

  private var rectPosX = 0
  private var rectPosY = 0

  canvas.width <== width
  canvas.height <== height
  canvas.width.onChange(redraw())
  canvas.height.onChange(redraw())

  def redraw() {
    draw()
  }

  private def draw() {
    val size = MinimapSize(field.width, field.height, width.value.toInt, height.value.toInt)

    val w = width.value
    val h = height.value
    if (w == 0 || h == 0) {
      return ;
    }

    val xOffset = (w - size.minimapUsefulWidth) / 2
    val yOffset = (h - size.minimapUsefulHeight) / 2

    val gc = canvas.graphicsContext2D
    gc.save()
    gc.fill = Color.BLACK
    gc.fillRect(0, 0, w, h)

    val side = size.cellSide
    field.hexes.foreach { hex =>
      val x = hex.x * side + xOffset
      val offset = if (hex.x % 2 == 0) 0 else side / 2
      val y = hex.y * side + offset + yOffset
      gc.fill = color(hex)
      gc.fillRect(x, y, side, side)
    }

    gc.stroke = Color.GRAY
    gc.strokeRect(xOffset, yOffset, size.minimapUsefulWidth, size.minimapUsefulHeight)

    gc.stroke = Color.WHITE
    gc.strokeRect(rectPosX, rectPosY, rectWidth.toDouble, rectHeight.toDouble)

    gc.restore()
  }

  def clickOnMinimap(x: Int, y: Int) {
    if (rectWidth.intValue == 0 || rectHeight.intValue == 0) {
      return
    }

    rectPosX = x - rectWidth.intValue / 2
    rectPosY = y - rectHeight.intValue / 2

    if (rectPosY < 0) {
      rectPosY = 0
    }

    if (rectPosY < 0) {
      rectPosY = 0
    }

    if (rectPosX + rectWidth.intValue > this.width.value) {
      rectPosX = this.width.value.toInt - rectWidth.intValue
    }

    if (rectPosY + rectHeight.intValue > this.height.value) {
      rectPosY = this.height.value.toInt - rectHeight.intValue
    }

    updatePositionOnMap()
  }

  def updatePositionOnMinimap() {
    val y = pane.vvalue.value.toInt
    val x = pane.hvalue.value.toInt
    val coords = positionOnMapToMinimap(x, y)
    rectPosX = coords._1
    rectPosY = coords._2
    redraw()
  }

  def updatePositionOnMap() {
    val coords = positionOnMinimapToMap(rectPosX, rectPosY)
    pane.hvalue.value = coords._1
    pane.vvalue.value = coords._2
  }

  pane.hvalue.onChange {
    if (updateOnPaneChange) {
      updatePositionOnMinimap()
    }
  }

  pane.vvalue.onChange {
    if (updateOnPaneChange) {
      updatePositionOnMinimap()
    }
  }

  private def positionOnMapToMinimap(x: Int, y: Int): (Int, Int) = {
    if (mapToMinimapWidthProportion.doubleValue == 0 || mapToMinimapWidthProportion.doubleValue == 0) {
      (0, 0)
    } else {
      val newX = x / mapToMinimapWidthProportion.doubleValue()
      val newY = y / mapToMinimapWidthProportion.doubleValue()
      (newX.toInt, newY.toInt)
    }
  }

  private def positionOnMinimapToMap(x: Int, y: Int): (Int, Int) = {
    val newX = x * mapToMinimapWidthProportion.doubleValue()
    val newY = y * mapToMinimapWidthProportion.doubleValue()
    (newX.toInt, newY.toInt)
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
