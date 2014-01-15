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

object TerrainHexView {
  val Side = 72
  val textLength = 36

  lazy val movementImpossibleImage: Image = {
    drawImage(Side, Side) { gc =>
      gc.globalAlpha = 0.3
      gc.fill = Color.BLACK
      gc.fillPolygon(angles())
    }
  }

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

  private val terrainTypeImageCache = collection.mutable.Map[(TerrainType, Option[Direction]), Image]()
  private def terrainTypeImage(terrain: TerrainType, direction: Option[Direction]): Image = {
    if (!terrainTypeImageCache.contains(terrain, direction)) {
      val terrainImage = MImage(terrain.imagePath).image
      val mask = maskForImage(terrainImage, direction)
      val bufferedImage = SwingFXUtils.fromFXImage(terrainImage, null)
      val afterMasking = leaveMaskedAreaOnly(bufferedImage, mask)
      val cut = cutTerrainHex(afterMasking, direction)
      val image = new Image(SwingFXUtils.toFXImage(cut, null))
      terrainTypeImageCache += (terrain, direction) -> image
    }

    terrainTypeImageCache(terrain, direction)
  }

  // will use mask to render on it
  private def leaveMaskedAreaOnly(originalImage: BufferedImage, mask: BufferedImage): BufferedImage = {
    val ac = AlphaComposite.getInstance(AlphaComposite.SRC_IN, 1.0F)
    val g = mask.getGraphics().asInstanceOf[Graphics2D]
    g.setComposite(ac)
    g.drawImage(originalImage, 0, 0, null)
    mask
  }

  private def maskForImage(image: Image, direction: Option[Direction]): BufferedImage = {
    val center = centerByDirection(direction, image.width.value.toInt, image.height.value.toInt)
    val result = maskImage(center._1, center._2, image.width.value.toInt,
      image.height.value.toInt)

    result
  }

  private def centerByDirection(direction: Option[Direction], width: Int, height: Int): (Int, Int) = {
    val map = Map(N -> (0, -Side), NE -> (Side * 3 / 4, -Side / 2),
      SE -> (Side * 3 / 4, Side / 2), S -> (0, Side), SW -> (-Side * 3 / 4, Side / 2),
      NW -> (-Side * 3 / 4, -Side / 2))

    val center = (width / 2, height / 2)
    direction match {
      case None => center
      case Some(dir) => {
        val m = map(dir)
        (center._1 + m._1, center._2 + m._2)
      }
    }
  }

  /**
   * hex with center in cords is black, other things are transparent
   */
  private def maskImage(centerX: Int, centerY: Int, width: Int, height: Int, pix: Int = 0): BufferedImage = {
    val topLeftX = centerX - Side / 2
    val topLeftY = centerY - Side / 2
    val angles = anglesWithCorrection(topLeftX, topLeftY, pix)

    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB_PRE)
    val g = image.getGraphics()
    g.setColor(java.awt.Color.BLACK)
    g.fillPolygon(angles.map(_._1.toInt).toArray, angles.map(_._2.toInt).toArray, angles.size)
    image
  }

  private def cutTerrainHex(image: BufferedImage, direction: Option[Direction]): BufferedImage = {
    val center = centerByDirection(direction, image.getWidth(), image.getHeight())
    val retImage = new BufferedImage(Side, Side, BufferedImage.TYPE_INT_ARGB_PRE)
    val g = retImage.getGraphics()
    g.drawImage(image, 0, 0, Side, Side, center._1 - Side / 2, center._2 - Side / 2, center._1 + Side / 2, center._2 + Side / 2, null)
    retImage
  }
}

class TerrainHexView(val hex: TerrainHex, field: TerrainHexField, fieldView: TerrainHexFieldView) {
  private val arrowPath = "/images/arrows/"

  val neighbours = field.neighboursWithDirections(hex.x, hex.y)
  val directions = neighbours map (p => (p._2, p._1)) toMap
  val side = TerrainHexView.Side
  val x = findX
  val y = findY
  var isDirty = true
  var isDarkened = false
  private var currentDarkened = false
  private var _arrowStart: Option[Direction] = None
  def arrowStart = _arrowStart
  def arrowStart_=(as: Option[Direction]) {
    if (as != _arrowStart) {
      _arrowStart = as
      isDirty = true
    }
  }
  private var _arrowEnd: Option[Direction] = None
  def arrowEnd = _arrowEnd
  def arrowEnd_=(ae: Option[Direction]) {
    if (ae != _arrowEnd) {
      _arrowEnd = ae
      isDirty = true
    }
  }

  private var _defence: Option[(SoldierDefence, Boolean)] = None
  def defence = _defence
  def defence_=(d: Option[(SoldierDefence, Boolean)]) {
    if (_defence != d) {
      _defence = d
      isDirty = true
    }
  }

  private var _soldier: Option[SoldierView] = None
  def soldier = _soldier
  def soldier_=(s: Option[SoldierView]) {
    if (_soldier != s) {
      _soldier.foreach(_.hexView = None)
      _soldier = s
      _soldier.foreach(_.hexView = Some(this))
      isDirty = true
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

  private lazy val neighbourParts: List[Image] = {
    val neighboursWithDirections = fieldView.neighboursWithDirections(this)
    neighboursWithDirections.toList.map { case (d, n) => TerrainHexView.terrainTypeImage(n.hex.terrain, Some(d.opposite)) }
  }

  val image: Image = {
    if (hex.terrain == Forest) {
      TerrainHexView.terrainTypeImage(Grass, None)
    } else {
      TerrainHexView.terrainTypeImage(hex.terrain, None)
    }
  }

  val secondaryImage: Option[Image] = {
    if (hex.terrain == Forest) {
      Some(TerrainHexView.terrainTypeImage(Forest, None))
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

  // TODO cache it based on neighbors, elements, etc
  lazy val hexImage: Image = {
    drawImage(TerrainHexView.Side, TerrainHexView.Side) { gc =>
      gc.drawImage(image, 0, 0)
      elements foreach (_.drawItself(gc, 0, 0))
      neighbourMapObjects foreach (_.drawImage(gc, 0, 0))
      secondaryImage.foreach(gc.drawImage(_, 0, 0))
      mapObject foreach (_.drawImage(gc, 0, 0))
      neighbourParts.foreach(gc.drawImage(_, 0, 0))
    }
  }

  def drawItself(gc: GraphicsContext) {
    if (isDirty || isDarkened != currentDarkened) {
      gc.drawImage(hexImage, x, y)
      drawHexGrid(gc)
      drawSoldierIfNeeded(gc)
      drawMovementImpossibleIfNeeded(gc)
      drawArrowStartIfNeeded(gc)
      drawArrowEndIfNeeded(gc)
      drawDefenceIfNeeded(gc)
      isDirty = false
    }

  }

  private def drawMovementImpossibleIfNeeded(gc: GraphicsContext) {
    if (currentDarkened != isDarkened) {
      if (isDarkened) {
        gc.drawImage(TerrainHexView.movementImpossibleImage, x, y)
      }
      currentDarkened = isDarkened
    }
  }

  private def drawArrowStartIfNeeded(gc: GraphicsContext) {
    arrowStart foreach { dir =>
      val path = arrowPath + "attack-indicator-src-" + dir.toString.toLowerCase() + ".png"
      MImage(path).drawImage(gc, x, y)
    }
  }

  private def drawArrowEndIfNeeded(gc: GraphicsContext) {
    arrowEnd foreach { dir =>
      val path = arrowPath + "attack-indicator-dst-" + dir.toString.toLowerCase() + ".png"
      MImage(path).drawImage(gc, x, y)
    }
  }

  private def drawDefenceIfNeeded(gc: GraphicsContext) {
    defence foreach {
      case (d, drawPolygon) =>
        val image = TerrainHexView.defenceImages(d.defence, drawPolygon)
        gc.drawImage(image, x, y)
    }
  }

  private def drawSoldierIfNeeded(gc: GraphicsContext) {
    soldier foreach { s =>
      s.drawItself(gc)
    }
  }

  private def drawHexGrid(gc: GraphicsContext) {
    gc.drawImage(TerrainHexView.hexGridImage, x, y)
  }

  def center = (x + side / 2, y + side / 2)
  def coords = (x, y)
}