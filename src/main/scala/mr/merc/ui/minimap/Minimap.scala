package mr.merc.ui.minimap

import scalafx.scene.canvas.Canvas
import mr.merc.map.hex.TerrainHexField
import scalafx.scene.paint.Color
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import javafx.beans.property.SimpleDoubleProperty

import scala.beans.BeanProperty
import scalafx.scene.layout.{Pane, VBox}
import scalafx.scene.control.ScrollPane
import mr.merc.map.view.MapView
import scalafx.beans.binding.Bindings._
import javafx.scene.{input => jfxin}
import mr.merc.image.ImageUtil
import mr.merc.ui.common.ConversionUtils._
import mr.merc.ui.common.{ImageHelper, ScrollPaneLike}
import scalafx.scene.image.Image
import scalafx.Includes._
import scalafx.scene.input.MouseEvent

class Minimap(field: TerrainHexField, pane: ScrollPaneLike, factor: Double) extends Pane {
  private val mapCanvas = new Canvas()
  private val scrollCanvas = new Canvas()
  children = List(mapCanvas, scrollCanvas)
  private val mapView = new MapView(field, factor)
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

  mapCanvas.width <== this.width
  mapCanvas.height <== this.height
  mapCanvas.layoutX = 0
  mapCanvas.layoutY = 0
  scrollCanvas.width <== this.width
  scrollCanvas.height <== this.height
  scrollCanvas.layoutX = 0
  scrollCanvas.layoutY = 0

  this.width.onChange(refreshMapCanvas())
  this.height.onChange(refreshMapCanvas())

  scrollCanvas.onMouseClicked = canvasMouseClicked _
  scrollCanvas.onMouseDragged = canvasMouseClicked _

  def minimapSize = MinimapSize(field.width, field.height, width.intValue, height.intValue)

  private def refreshMapCanvas() {
    val gc = mapCanvas.graphicsContext2D
    gc.save()
    gc.fill = Color.Black
    gc.fillRect(0, 0, width.value, height.value)
    val side = minimapSize.cellSide
    field.hexes.foreach { hex =>
      val x = hex.x * side + xOffset
      val offset = if (hex.x % 2 == 0) 0 else side / 2
      val y = hex.y * side + offset.ceil + yOffset
      gc.fill = color(hex)
      gc.fillRect(x, y, side, side)
      hex.province.foreach { p =>
        if (hex.terrain.isNot(WaterKind)) {
          gc.fill = p.owner.color.opacity(0.7f)
          gc.fillRect(x, y, side, side)
        }
      }
    }
    gc.restore()
    redraw()
  }

  def redraw() {
    val w = width.value
    val h = height.value
    if (w == 0 || h == 0) {
      return
    }

    val gc = scrollCanvas.graphicsContext2D
    gc.save()
    gc.clearRect(0, 0, w, h)
    gc.stroke = Color.Gray
    gc.strokeRect(xOffset, yOffset, minimapSize.minimapUsefulWidth, minimapSize.minimapUsefulHeight)

    gc.stroke = Color.White
    gc.lineWidth = 2
    gc.strokeRect(xOffset + rectPosX, yOffset + rectPosY,
      rectWidth, rectHeight)

    gc.restore()
  }


  private def canvasMouseClicked(event: MouseEvent) {
    val x = event.x.toInt
    val y = event.y.toInt
    clickOnMinimap(x, y)
    redraw()
    event.consume()
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

    if (rectPosY + rectHeight > minimapSize.minimapUsefulHeight) {
      rectPosY = minimapSize.minimapUsefulHeight - rectHeight
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

  private def neighbourFromOtherOwner(hex: TerrainHex): Boolean = {
    field.neighbours(hex).exists { n =>
      val opt = for {
        thisProvince <- hex.province
        thatProvince <- n.province
      } yield {
        thisProvince.owner != thatProvince.owner
      }
      opt.getOrElse(false)
    }
  }

  private def smallerNeighbourFromCurrentOwner(hex: TerrainHex): Boolean = {
    field.neighbours(hex).exists { n =>
      val opt = for {
        thisProvince <- hex.province
        thatProvince <- n.province
      } yield {
        thisProvince.owner == thatProvince.owner && thisProvince.toString > thatProvince.toString
      }
      opt.getOrElse(false)
    }
  }

  private def color(hex: TerrainHex): Color = {
    if (hex.soldier.nonEmpty) {
      hex.soldier.get.owner.color
    } else {
      hex.terrain.kind match {
        case GrassKind => Color.Green
        case WaterKind => Color.Blue
        case HillKind => Color.Gray
        case MountainKind => Color.Black
        case SandKind => Color.Yellow
        case SwampKind => Color.Brown
        case ForestKind => Color.DarkGreen
        case RoadKind => Color.LightGray
        case _ => Color.Black
      }
    }
  }
}
