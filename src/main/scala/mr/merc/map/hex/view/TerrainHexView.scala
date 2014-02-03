package mr.merc.map.hex.view

import scalafx.scene.canvas.GraphicsContext
import mr.merc.image.MImage
import mr.merc.map.terrain.Grass
import mr.merc.map.terrain.Forest
import mr.merc.map.hex.TerrainHex
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.CubeHex
import scalafx.scene.paint.Color
import mr.merc.map.hex.Direction
import mr.merc.image.MImageCache
import mr.merc.unit.SoldierDefence
import scalafx.scene.text.Font
import scalafx.scene.text.FontWeight
import scalafx.scene.image.Image
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.WritableImage
import scalafx.scene.SnapshotParameters
import mr.merc.ui.common.ImageHelper._
import mr.merc.unit.SoldierDefence
import mr.merc.unit.view.SoldierView
import mr.merc.map.terrain.TerrainType
import javafx.embed.swing.SwingFXUtils
import java.awt.image.BufferedImage
import java.awt.AlphaComposite
import java.awt.Graphics2D
import mr.merc.map.hex._
import javax.imageio.ImageIO
import java.io.File
import java.util.UUID
import mr.merc.map.objects.MapObject
import scalafx.scene.effect.BlendMode

object TerrainHexView {
  val Side = 72
  val textLength = 36

  var imageCache = collection.mutable.HashMap[(TerrainType, Option[MapObject], Map[Direction, TerrainType], Map[Direction, MapObject]), Image]()

  lazy val hexGridImage: Image = {
    drawImage(Side, Side) { gc =>
      gc.stroke = Color.BLACK
      gc.strokePolygon(angles())
    }
  }

  private def angles(pix: Int = 0): Seq[(Double, Double)] = {
    val a = Side / 4 toDouble
    val coef = Seq((1, 0), (3, 0), (4, 2), (3, 4), (1, 4), (0, 2))
    val pixelCorrection = Seq((1, 1), (-1, 1), (-1, 0), (-1, -1), (1, -1), (1, 0))
    (coef zip pixelCorrection).map { case ((x, y), (px, py)) => (x * a + px * pix, y * a + py * pix) }
  }

  private def anglesWithCorrection(xCorr: Int, yCorr: Int, pix: Int = 0): Seq[(Double, Double)] = {
    angles(pix).map { case (x, y) => (x + xCorr, y + yCorr) }
  }

  lazy val defenceImages: Map[(Int, Boolean), Image] = {
    val list = for (d <- 0 to 100 by 10; drawPolygon <- List(true, false)) yield {
      ((d, drawPolygon), drawImage(Side, Side) { gc =>
        if (drawPolygon) {
          gc.stroke = Color.YELLOW
          gc.lineWidth = 2
          gc.strokePolygon(angles(2))
        }

        gc.font = Font.font(Font.default.getFamily, FontWeight.NORMAL, 30)
        gc.fill = defenceColor(d)
        gc.fillText(d.toString, Side / 2 - textLength / 2, Side / 2 + 10, textLength)
      })
    }

    list toMap
  }

  private def defenceColor(d: Int): Color = {
    if (d <= 20) {
      Color.DARKRED
    } else if (d <= 30) {
      Color.RED
    } else if (d <= 40) {
      Color.YELLOW
    } else if (d <= 50) {
      Color.LIGHTGREEN
    } else if (d <= 60) {
      Color.GREEN
    } else {
      Color.GREEN
    }
  }

  lazy val movementImpossibleImage: Image = {
    drawImage(Side, Side) { gc =>
      gc.globalAlpha = 0.6
      gc.fill = Color.BLACK
      gc.fillPolygon(angles())
    }
  }
}

class TerrainHexView(val hex: TerrainHex, field: TerrainHexField, fieldView: TerrainHexFieldView) {
  private val arrowPath = "/images/arrows/"

  val neighbours = field.neighboursWithDirections(hex.x, hex.y)
  val directions = neighbours map (p => (p._2, p._1)) toMap
  val side = TerrainHexView.Side
  val x = findX
  val y = findY
  var isInterfaceDirty = true
  var isDarkened = false
  private var currentDarkened = false
  private var _arrowStart: Option[Direction] = None
  def arrowStart = _arrowStart
  def arrowStart_=(as: Option[Direction]) {
    if (as != _arrowStart) {
      _arrowStart = as
      isInterfaceDirty = true
    }
  }
  private var _arrowEnd: Option[Direction] = None
  def arrowEnd = _arrowEnd
  def arrowEnd_=(ae: Option[Direction]) {
    if (ae != _arrowEnd) {
      _arrowEnd = ae
      isInterfaceDirty = true
    }
  }

  private var _defence: Option[(SoldierDefence, Boolean)] = None
  def defence = _defence
  def defence_=(d: Option[(SoldierDefence, Boolean)]) {
    if (_defence != d) {
      _defence = d
      isInterfaceDirty = true
    }
  }

  override def toString = s"TerrainHexView[coords=($hex.x, $hex.y), pixels=($x,$y)]"

  private def findX = if (hex.x % 2 == 0) {
    hex.x * 6 * side / 8
  } else {
    (hex.x - 1) * 6 * side / 8 + side * 3 / 4
  }

  private def findY = if (hex.x % 2 == 0) {
    side * hex.y
  } else {
    side * hex.y + side / 2
  }

  private lazy val elements: List[TerrainHexViewAdditiveElement] = {
    val additives = TerrainHexViewAdditive.extractAdditives(this)
    val rule = new TerrainHexViewAdditiveRule
    rule.transform(additives)
  }

  val image: MImage = {
    if (hex.terrain == Forest) {
      MImage(Grass.imagePath)
    } else {
      MImage(hex.terrain.imagePath)
    }
  }

  val secondaryImage: Option[MImage] = {
    if (hex.terrain == Forest) {
      Some(MImage(Forest.imagePath))
    } else {
      None
    }
  }

  val mapObject = hex.mapObj match {
    case Some(mapObj) => mapObj.images(hex, field)
    case None => Nil
  }

  val neighbourMapObjects: List[MImage] = {
    val neigMapObj = field.neighbours(hex).filter(p => p.mapObj != None && p.mapObj != hex.mapObj)
    neigMapObj.flatMap(p => p.mapObj.get.images(hex, field)).toList
  }

  def drawTerrainImage(gc: GraphicsContext)(implicit offset: (Int, Int)) {
    val side = TerrainHexView.Side
    val x = this.x + offset._1
    val y = this.y + offset._2

    image.drawCenteredImage(gc, x, y, side, side)
    elements foreach (_.drawItself(gc, x, y))
    neighbourMapObjects foreach (_.drawImage(gc, x, y))
    secondaryImage.foreach(_.drawCenteredImage(gc, x, y, side, side))
    mapObject foreach (_.drawImage(gc, x, y))
  }

  def darkeningShouldBeRedrawn = isDarkened != currentDarkened

  def drawItself(gc: GraphicsContext, stage: HexDrawingStage, xOffset: Int, yOffset: Int) {
    implicit val offset = (xOffset, yOffset)
    stage match {
      case TerrainImageStage =>
        drawTerrainImage(gc)
      case MovementImpossibleStage =>
        drawMovementImpossibleIfNeeded(gc)
      case ArrowStage =>
        drawArrowStartIfNeeded(gc)
        drawArrowEndIfNeeded(gc)
      case DefenceStage =>
        drawDefenceIfNeeded(gc)
      case HexGridStage =>
        drawHexGrid(gc)
      case EndInterfaceDrawing =>
        isInterfaceDirty = false
      case ClearStage =>
        drawClearStage(gc)
    }
  }

  private def drawClearStage(gc: GraphicsContext) {
    gc.clearRect(x, y, TerrainHexView.Side, TerrainHexView.Side)
  }

  private def drawMovementImpossibleIfNeeded(gc: GraphicsContext)(implicit offset: (Int, Int)) {
    if (isDarkened && !arrowEnd.isDefined) {
      gc.drawImage(TerrainHexView.movementImpossibleImage, x + offset._1, y + offset._2)
    }
    currentDarkened = isDarkened
  }

  private def drawArrowStartIfNeeded(gc: GraphicsContext)(implicit offset: (Int, Int)) {
    arrowStart foreach { dir =>
      val path = arrowPath + "attack-indicator-src-" + dir.toString.toLowerCase() + ".png"
      MImage(path).drawImage(gc, x + offset._1, y + offset._2)
    }
  }

  private def drawArrowEndIfNeeded(gc: GraphicsContext)(implicit offset: (Int, Int)) {
    arrowEnd foreach { dir =>
      val path = arrowPath + "attack-indicator-dst-" + dir.toString.toLowerCase() + ".png"
      MImage(path).drawImage(gc, x + offset._1, y + offset._2)
    }
  }

  private def drawDefenceIfNeeded(gc: GraphicsContext)(implicit offset: (Int, Int)) {
    defence foreach {
      case (d, drawPolygon) =>
        val image = TerrainHexView.defenceImages(d.defence, drawPolygon)
        gc.drawImage(image, x + offset._1, y + offset._2)
    }
  }

  private def drawHexGrid(gc: GraphicsContext)(implicit offset: (Int, Int)) {
    gc.drawImage(TerrainHexView.hexGridImage, x + offset._1, y + offset._2)
  }

  def center = (x + side / 2, y + side / 2)
  def coords = (x, y)
}

sealed trait HexDrawingStage
case object ClearStage extends HexDrawingStage
case object TerrainImageStage extends HexDrawingStage
case object MovementImpossibleStage extends HexDrawingStage
case object ArrowStage extends HexDrawingStage
case object DefenceStage extends HexDrawingStage
case object HexGridStage extends HexDrawingStage
case object EndInterfaceDrawing extends HexDrawingStage
