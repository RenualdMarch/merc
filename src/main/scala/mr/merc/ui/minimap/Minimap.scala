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
import javafx.scene.{ input => jfxin }
import mr.merc.ui.common.ConversionUtils._
import mr.merc.ui.common.ScrollPaneLike

class Minimap(field: TerrainHexField, pane: ScrollPaneLike) extends VBox {
  private val canvas = new Canvas()
  content = canvas
  private val mapView = new MapView(field)
  private var updateOnPaneChange = true

  private def xOffset = (width.value - minimapSize.minimapUsefulWidth) / 2
  private def yOffset = (height.value - minimapSize.minimapUsefulHeight) / 2

  private def visiblePartWidth = Math.min(1, pane.width.value / mapView.pixelWidth)
  private def visiblePartHeight = Math.min(1, pane.height.value / mapView.pixelHeight)
  private def rectWidth = visiblePartWidth * minimapSize.minimapUsefulWidth
  private def rectHeight = visiblePartHeight * minimapSize.minimapUsefulHeight
  private def scrollableWidth = minimapSize.minimapUsefulWidth - rectWidth
  private def scrollableHeight = minimapSize.minimapUsefulHeight - rectHeight

  private var rectPosX = 0d
  private var rectPosY = 0d

  canvas.width <== width
  canvas.height <== height
  canvas.width.onChange(redraw())
  canvas.height.onChange(redraw())

  def minimapSize = MinimapSize(field.width, field.height, width.value.toInt, height.value.toInt)

  def redraw() {
    val w = width.value
    val h = height.value
    if (w == 0 || h == 0) {

      return ;
    }

    val gc = canvas.graphicsContext2D
    gc.save()
    gc.fill = Color.BLACK
    gc.fillRect(0, 0, w, h)

    val side = minimapSize.cellSide
    field.hexes.foreach { hex =>
      val x = hex.x * side + xOffset
      val offset = if (hex.x % 2 == 0) 0 else side / 2
      val y = hex.y * side + offset.ceil + yOffset
      gc.fill = color(hex)
      gc.fillRect(x, y, side, side)
    }

    gc.stroke = Color.GRAY
    gc.strokeRect(xOffset, yOffset, minimapSize.minimapUsefulWidth, minimapSize.minimapUsefulHeight)

    gc.stroke = Color.WHITE
    gc.lineWidth = 2
    gc.strokeRect(xOffset + rectPosX, yOffset + rectPosY,
      rectWidth, rectHeight)

    gc.restore()
  }

  canvas.delegate.addEventHandler(jfxin.MouseEvent.MOUSE_CLICKED, canvasMouseClicked _)
  canvas.delegate.addEventHandler(jfxin.MouseEvent.MOUSE_DRAGGED, canvasMouseClicked _)

  private def canvasMouseClicked(event: jfxin.MouseEvent) {
    val x = event.getX().toInt
    val y = event.getY().toInt
    clickOnMinimap(x, y)
    redraw()
  }

  def clickOnMinimap(x: Int, y: Int) {
    if (rectWidth == 0 || rectHeight == 0) {
      return
    }

    rectPosX = x - xOffset - rectWidth / 2
    rectPosY = y - yOffset - rectHeight / 2

    if (rectPosX + rectWidth > minimapSize.minimapUsefulWidth) {
      rectPosX = minimapSize.minimapUsefulWidth - rectWidth
    }

    if (rectPosY + rectHeight.intValue > minimapSize.minimapUsefulHeight) {
      rectPosY = minimapSize.minimapUsefulHeight - rectHeight.intValue
    }

    if (rectPosX < 0) {
      rectPosX = 0
    }

    if (rectPosY < 0) {
      rectPosY = 0
    }

    updatePositionOnMap()
  }

  def updatePositionOnMinimap() {
    val y = pane.vvalue.value
    val x = pane.hvalue.value
    val coords = positionOnMapToMinimap(x, y)
    rectPosX = coords._1
    rectPosY = coords._2
    redraw()
  }

  def updatePositionOnMap() {
    val coords = positionOnMinimapToMap(rectPosX, rectPosY)
    updateOnPaneChange = false
    pane.hvalue.value = coords._1
    pane.vvalue.value = coords._2
    updateOnPaneChange = true
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

  private def positionOnMapToMinimap(w: Double, h: Double): (Int, Int) = {
    val newX = scrollableWidth * w
    val newY = scrollableHeight * h
    (newX.toInt, newY.toInt)
  }

  private def positionOnMinimapToMap(x: Double, y: Double): (Double, Double) = {
    val w = if (scrollableWidth == 0) {
      0
    } else {
      rectPosX.toDouble / scrollableWidth
    }

    val h = if (scrollableHeight == 0) {
      0
    } else {
      rectPosY.toDouble / scrollableHeight
    }

    (w, h)
  }

  private def color(hex: TerrainHex): Color = {
    if (hex.soldier.nonEmpty) {
      hex.soldier.get.owner.color
    } else {
      Color.BLACK
    }
  }
}
