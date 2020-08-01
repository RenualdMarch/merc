package mr.merc.map.hex.view

import scalafx.scene.canvas.GraphicsContext
import mr.merc.image.MImage
import mr.merc.map.terrain._
import mr.merc.map.hex._
import scalafx.scene.paint.Color
import scalafx.scene.text.Font
import scalafx.scene.text.FontWeight
import scalafx.scene.image.Image
import mr.merc.ui.common.ImageHelper._
import mr.merc.unit.SoldierDefence
import mr.merc.ui.common.geom.Line
import mr.merc.util.CacheFactoryMap


object TerrainHexView {

  def side(factor: Double): Int = (72 * factor) toInt

  def factor(side: Double): Double = side / 72

  def textLength(factor: Double): Int = side(factor) / 2

  private val hexGridImageCache = new CacheFactoryMap[Int, Image](
    side =>
      drawImage(side, side) { gc =>
        gc.stroke = Color.Black
        gc.lineWidth = 0.5 * factor(side)
        gc.strokePolygon(angles(side))
      })


  private[this] def angles(side: Int, pix: Int = 0): Seq[(Double, Double)] = {
    val a = side / 4.0
    val coef = Seq((1, 0), (3, 0), (4, 2), (3, 4), (1, 4), (0, 2))
    val pixelCorrection = Seq((1, 1), (-1, 1), (-1, 0), (-1, -1), (1, -1), (1, 0))
    (coef zip pixelCorrection).map { case ((x, y), (px, py)) => (x * a + px * pix, y * a + py * pix) }
  }

  private def pointsWithDirections(side: Int, pix: Int): Map[Direction, Line] = {
    val points = angles(side, pix).toList
    val cycledPoints = points.tail ::: List(points.head)
    Direction.list.zip(points.zip(cycledPoints)).map {
      case (dir, ((x1, y1), (x2, y2))) => dir -> Line(x1, y1, x2, y2)
    } toMap
  }

  private val defenceImagesCache = new CacheFactoryMap[Int, Map[(Int, Boolean), Image]](defenceImages)

  private[this] def defenceImages(side: Int): Map[(Int, Boolean), Image] = {
    val textLength = side / 2
    val list = for (d <- 0 to 100 by 10; drawPolygon <- List(true, false)) yield {
      ((d, drawPolygon), drawImage(side, side) { gc =>
        if (drawPolygon) {
          gc.stroke = defenceColor(d)
          gc.lineWidth = 2
          gc.strokePolygon(angles(side))
        }

        gc.font = Font.font(Font.default.getFamily, FontWeight.Normal, side / 2.5)
        gc.fill = defenceColor(d)
        gc.fillText(d.toString, side / 2 - textLength / 2, side / 2 + side / 7, textLength)
      })
    }

    list toMap
  }

  private[this] def defenceColor(d: Int): Color = {
    if (d <= 20) {
      Color.DarkRed
    } else if (d <= 30) {
      Color.Red
    } else if (d <= 40) {
      Color.Yellow
    } else if (d <= 50) {
      Color.LightGreen
    } else if (d <= 60) {
      Color.Green
    } else {
      Color.Green
    }
  }

  private val movementImpossibleCache = new CacheFactoryMap[Int, Image](
    side =>
      drawImage(side, side) { gc =>
        gc.globalAlpha = 0.6
        gc.fill = Color.Black
        gc.fillPolygon(angles(side))
      }
  )
}

class TerrainHexView(val hex: TerrainHex, field: TerrainHexField, fieldView: TerrainHexFieldView, val factor: Double) {
  private val arrowPath = "/images/arrows/"

  val side: Int = TerrainHexView.side(factor)
  val textLength: Int = TerrainHexView.textLength(factor)

  val neighbours: Map[Direction, TerrainHex] = field.neighboursWithDirections(hex.x, hex.y)
  val directions: Map[TerrainHex, Direction] = neighbours map (p => (p._2, p._1))
  val x: Int = findX
  val y: Int = findY
  private var _terrainDirty = false

  def terrainDirty: Boolean = _terrainDirty

  // to be called only from terrain hex field view !!!!!
  private[view] def terrainDirty_=(v: Boolean): Unit = {
    if (!terrainDirty && v) {
      this._terrainDirty = v
      if (thisHexViewIsHuge()) {
        fieldView.neighbours(this).foreach {
          _.terrainDirty = v
        }
      }
    } else {
      this._terrainDirty = v
    }

  }

  def recalculateTerrainDirtIfNeeded(): Unit = {
    if (terrainDirty) {
      this.elements = calculateElements()
      this.sameOwnerBorders = calculateSameOwnerBorders()
      this.differentOwnerBorders = calculateDifferentOwnerBorders()
      terrainDirty = false
    }
  }

  var _animationDirty = false

  // to be called only from terrain hex field view !!!!!
  def animationDirty: Boolean = _animationDirty

  private[view] def animationDirty_=(v: Boolean): Unit = {
    if (v && !animationDirty) {
      _animationDirty = v
      if (thisHexViewIsHuge()) {
        fieldView.neighbours(this).foreach {
          _.animationDirty = v
        }
      }
    }
  }

  private def thisHexViewIsHuge(): Boolean = {
    this.hex.terrain.isOneOf(MountainKind, ForestKind, WallsKind) || this.mapObjectView.nonEmpty
  }

  var interfaceDirty = true
  var darkened = false
  private var currentDarkened = darkened
  private var _arrowStart: Option[Direction] = None

  def arrowStart: Option[Direction] = _arrowStart

  def arrowStart_=(as: Option[Direction]) {
    if (as != _arrowStart) {
      _arrowStart = as
      interfaceDirty = true
    }
  }

  private var _arrowEnd: Option[Direction] = None

  def arrowEnd: Option[Direction] = _arrowEnd

  def arrowEnd_=(ae: Option[Direction]) {
    if (ae != _arrowEnd) {
      _arrowEnd = ae
      interfaceDirty = true
    }
  }

  private var _defence: Option[(SoldierDefence, Boolean)] = None

  def defence: Option[(SoldierDefence, Boolean)] = _defence

  def defence_=(d: Option[(SoldierDefence, Boolean)]) {
    if (_defence != d) {
      _defence = d
      interfaceDirty = true
    }
  }

  override def toString = s"TerrainHexView[coords=(${hex.x}, ${hex.y}), pixels=($x,$y)]"

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

  private var elements: List[TerrainHexViewAdditiveElement] = calculateElements()

  private def calculateElements(): List[TerrainHexViewAdditiveElement] = {
    val additives = TerrainHexViewAdditive.extractAdditives(this)
    val rule = new TerrainHexViewAdditiveRule
    rule.transform(additives)
  }

  lazy val image: MImage = {
    hex.terrain.belowTerrainType match {
      case Some(b) => b.image(hex.x, hex.y)
      case None => hex.terrain.image(hex.x, hex.y)
    }
  }

  lazy val secondaryImage: Option[MImage] = {
    hex.terrain.belowTerrainType.map { _ =>
      hex.terrain.image(hex.x, hex.y)
    }
  }

  lazy val mapObjectView: List[MImage] = hex.mapObj match {
    case Some(mapObj) => mapObj.view.images(hex, field)
    case None => Nil
  }

  lazy val neighbourMapObjects: List[MImage] = {
    val neigMapObj = field.neighbours(hex).filter(p => p.mapObj.isDefined && p.mapObj != hex.mapObj)
    neigMapObj.flatMap(p => p.mapObj.get.view.images(hex, field))
  }

  def drawCastleAndWalls(gc: GraphicsContext, xOffset: Int, yOffset: Int): Unit = {
    fieldView.castleImagesForHex(this.hex).foreach { wallImage =>
      wallImage.image.scaledImage(factor).drawImage(gc, this.x + xOffset, this.y + yOffset)
    }
  }

  def drawTerrainImage(gc: GraphicsContext, xOffset: Int, yOffset: Int) {
    val x = this.x + xOffset
    val y = this.y + yOffset


    image.scaledImage(factor).drawCenteredImage(gc, x, y, side, side)
    elements foreach (_.drawItself(gc, x, y, factor))

    neighbourMapObjects.foreach(_.scaledImage(factor).drawImage(gc, x, y))
  }

  def drawMapObjectIfNeeded(gc: GraphicsContext, xOffset: Int, yOffset: Int): Unit = {
    val x = this.x + xOffset
    val y = this.y + yOffset
    mapObjectView foreach (_.scaledImage(factor).drawCenteredImage(gc, x, y, side, side))
  }

  def drawSecondaryImageIfNeeded(gc: GraphicsContext, xOffset: Int, yOffset: Int): Unit = {
    val x = this.x + xOffset
    val y = this.y + yOffset
    secondaryImage.foreach(_.scaledImage(factor).drawCenteredImage(gc, x, y, side, side))
  }

  def darkeningShouldBeRedrawn: Boolean = darkened != currentDarkened

  def drawItself(gc: GraphicsContext, stage: HexDrawingStage, xOffset: Int, yOffset: Int) {
    recalculateTerrainDirtIfNeeded()
    animationDirty = false
    stage match {
      case TerrainImageStage =>
        drawTerrainImage(gc, xOffset, yOffset)
      case MovementImpossibleStage =>
        drawMovementImpossibleIfNeeded(gc, xOffset, yOffset)
      case ArrowStage =>
        drawArrowStartIfNeeded(gc, xOffset, yOffset)
        drawArrowEndIfNeeded(gc, xOffset, yOffset)
      case DefenceStage =>
        drawDefenceIfNeeded(gc, xOffset, yOffset)
      case HexGridStage =>
        drawHexGrid(gc, xOffset, yOffset)
      case EndInterfaceDrawing =>
        interfaceDirty = false
      case ClearStage =>
        drawClearStage(gc, xOffset, yOffset)
      case ProvinceBordersStage =>
        drawProvinceBorder(gc, xOffset, yOffset)
      case BuildingsForestMountainsStage =>
        drawCastleAndWalls(gc, xOffset, yOffset)
        drawMapObjectIfNeeded(gc, xOffset, yOffset)
        drawSecondaryImageIfNeeded(gc, xOffset, yOffset)
    }
  }


  private val anotherProvince = hex.province.map { province =>
    val neigsWithDirs = field.neighboursWithDirections(hex)
    neigsWithDirs.filter { case (dir, n) =>
      n.province.exists(_ != province) && n.terrain.isNot(WaterKind)
    }
  }.getOrElse(Map())

  private def calculateSameOwnerBorders(): List[Direction] = {
    anotherProvince.filter { case (_, h) =>
      h.province.get.owner == hex.province.get.owner
    }.keys.toList
  }

  private def calculateDifferentOwnerBorders(): List[Direction] = {
    anotherProvince.filter { case (_, h) =>
      h.province.get.owner != hex.province.get.owner
    }.keys.toList
  }

  private var sameOwnerBorders = calculateSameOwnerBorders()
  private var differentOwnerBorders = calculateDifferentOwnerBorders()

  private def drawProvinceBorder(gc: GraphicsContext, xOffset: Int, yOffset: Int): Unit = {
    def drawLine(map: List[Direction], color: Color, lineWidth: Int): Unit = {
      map.foreach { direction =>
        val line = TerrainHexView.pointsWithDirections(side, lineWidth / 2)(direction)
        gc.save()
        gc.stroke = color
        gc.lineWidth = lineWidth
        gc.strokeLine(this.x + xOffset + line.beginX, line.beginY + this.y + yOffset, line.endX + this.x + xOffset, line.endY + this.y + yOffset)
        gc.restore()
      }
    }

    hex.province.foreach { province =>
      if (hex.terrain.isNot(WaterKind)) {
        drawLine(sameOwnerBorders, Color.Black, 1)
        drawLine(differentOwnerBorders, province.owner.color, 4)

      }
    }
  }

  private def drawClearStage(gc: GraphicsContext, xOffset: Int, yOffset: Int) {
    gc.clearRect(x + xOffset, y + yOffset, side, side)
  }

  private def drawMovementImpossibleIfNeeded(gc: GraphicsContext, xOffset: Int, yOffset: Int) {
    if (darkened && arrowEnd.isEmpty) {
      gc.drawImage(TerrainHexView.movementImpossibleCache(side), x + xOffset, y + yOffset)
    }
    currentDarkened = darkened
  }

  private def drawArrowStartIfNeeded(gc: GraphicsContext, xOffset: Int, yOffset: Int) {
    arrowStart foreach { dir =>
      val path = arrowPath + "attack-indicator-src-" + dir.toString.toLowerCase() + ".png"
      MImage(path).scaledImage(factor).drawImage(gc, x + xOffset, y + yOffset)
    }
  }

  private def drawArrowEndIfNeeded(gc: GraphicsContext, xOffset: Int, yOffset: Int) {
    arrowEnd foreach { dir =>
      val path = arrowPath + "attack-indicator-dst-" + dir.toString.toLowerCase() + ".png"
      MImage(path).scaledImage(factor).drawImage(gc, x + xOffset, y + yOffset)
    }
  }

  private def drawDefenceIfNeeded(gc: GraphicsContext, xOffset: Int, yOffset: Int) {
    defence foreach {
      case (d, drawPolygon) =>
        val image = TerrainHexView.defenceImagesCache(side)(d.defence, drawPolygon)
        gc.drawImage(image, x + xOffset, y + yOffset)
    }
  }

  private def drawHexGrid(gc: GraphicsContext, xOffset: Int, yOffset: Int) {
    gc.drawImage(TerrainHexView.hexGridImageCache(side), x + xOffset, y + yOffset)
  }

  def center: (Int, Int) = (x + side / 2, y + side / 2)

  def coords: (Int, Int) = (x, y)
}

sealed trait HexDrawingStage

case object ClearStage extends HexDrawingStage

case object TerrainImageStage extends HexDrawingStage

case object MovementImpossibleStage extends HexDrawingStage

case object ArrowStage extends HexDrawingStage

case object DefenceStage extends HexDrawingStage

case object HexGridStage extends HexDrawingStage

case object BuildingsForestMountainsStage extends HexDrawingStage

case object EndInterfaceDrawing extends HexDrawingStage

case object ProvinceBordersStage extends HexDrawingStage
