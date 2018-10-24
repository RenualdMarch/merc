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
import mr.merc.map.objects.House
import mr.merc.ui.common.geom.Line
import mr.merc.util.CacheFactoryMap


object TerrainHexView {

  def side(factor: Double): Int = (72 * factor) toInt

  def textLength(factor: Double): Int = side(factor) / 2

  private val hexGridImageCache = new CacheFactoryMap[Int, Image](
    side =>
    drawImage(side, side) { gc =>
      gc.stroke = Color.Black
      gc.lineWidth = 0.25
      gc.strokePolygon(angles(side))
    })


  private [this] def angles(side: Int, pix: Int = 0): Seq[(Double, Double)] = {
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

  private [this] def defenceImages(side: Int): Map[(Int, Boolean), Image] = {
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

  private [this] def defenceColor(d: Int): Color = {
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

  val side = TerrainHexView.side(factor)
  val textLength = TerrainHexView.textLength(factor)

  val neighbours = field.neighboursWithDirections(hex.x, hex.y)
  val directions = neighbours map (p => (p._2, p._1))
  val x = findX
  val y = findY
  private var _isTerrainDirty = false
  def isTerrainDirty: Boolean = _isTerrainDirty
  def isTerrainDirty_=(v: Boolean): Unit = {
    if(!_isTerrainDirty && v) {
      this.elements = calculateElements()
      this.sameOwnerBorders = calculateSameOwnerBorders()
      this.differentOwnerBorders = calculateDifferentOwnerBorders()
    }

    _isTerrainDirty = v
  }

  var _isAnimationDirty = false

  def isAnimationDirty = _isAnimationDirty
  def isAnimationDirty_=(v: Boolean): Unit = {
    if (v && !isAnimationDirty) {
      _isAnimationDirty = v
      fieldView.neighboursWithDirections(this).filter(neighbourShouldBeSetAnimationDirty _ tupled).foreach { case (_, view) =>
        view.isAnimationDirty = v
      }
    }

  }

  private def neighbourShouldBeSetAnimationDirty(dir: Direction, neig: TerrainHexView): Boolean = {
    if (!neig.isAnimationDirty) {
      // TODO implement me
      false
    } else false
  }

  var isInterfaceDirty = true
  var isDarkened = false
  private var currentDarkened = isDarkened
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

  var _provinceSelected = false
  def provinceSelected: Boolean = _provinceSelected
  def provinceSelected_=(ps: Boolean): Unit = {
    _provinceSelected = ps
    isInterfaceDirty = true
  }

  private var currentProvinceSelected = provinceSelected

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

  private var elements:List[TerrainHexViewAdditiveElement] = calculateElements()

  private def calculateElements(): List[TerrainHexViewAdditiveElement] = {
    val additives = TerrainHexViewAdditive.extractAdditives(this)
    val rule = new TerrainHexViewAdditiveRule
    rule.transform(additives)
  }

  def image: MImage = {
    if (hex.terrain.is(ForestKind)) {
      GreenGrass.image(hex.x, hex.y)
    } else {
      hex.terrain.image(hex.x, hex.y)
    }
  }

  def secondaryImage: Option[MImage] = {
    if (hex.terrain.is(ForestKind)) {
      Some(hex.terrain.image(hex.x, hex.y))
    } else {
      None
    }
  }

  def mapObject:List[MImage] = hex.mapObj match {
    case Some(mapObj) => mapObj.images(hex, field)
    case None => Nil
  }

  def neighbourMapObjects: List[MImage] = {
    val neigMapObj = field.neighbours(hex).filter(p => p.mapObj.isDefined && p.mapObj != hex.mapObj)
    neigMapObj.flatMap(p => p.mapObj.get.images(hex, field)).toList
  }

  def drawCastleAndWalls(gc: GraphicsContext, xOffset: Int, yOffset: Int): Unit = {
    fieldView.castleImagesForHex(this.hex).foreach { wallImage =>
      wallImage.image.scaledImage(factor).drawImage(gc, this.x + xOffset, this.y + yOffset)
    }
  }

  def drawTerrainImage(gc: GraphicsContext, xOffset: Int, yOffset: Int) {
    val x = this.x + xOffset
    val y = this.y + yOffset

    if (hex.terrain.is(MountainKind)) {
      elements.foreach (_.drawItself(gc, x, y, factor))
      image.scaledImage(factor).drawCenteredImage(gc, x, y, side, side)
    } else {
      image.scaledImage(factor).drawCenteredImage(gc, x, y, side, side)
      elements foreach (_.drawItself(gc, x, y, factor))
    }
    neighbourMapObjects.foreach (_.scaledImage(factor).drawImage(gc, x, y))

    if (!hex.mapObj.exists(_.isInstanceOf[House])) {
      mapObject foreach (_.scaledImage(factor).drawCenteredImage(gc, x, y, side, side))
    }
  }

  def drawHouseIfNeeded(gc: GraphicsContext, xOffset: Int, yOffset: Int): Unit = {
    if (hex.mapObj.exists(_.isInstanceOf[House])) {
      val x = this.x + xOffset
      val y = this.y + yOffset
      mapObject foreach (_.scaledImage(factor).drawCenteredImage(gc, x, y, side, side))
    }
  }

  def drawForestIfNeeded(gc: GraphicsContext, xOffset: Int, yOffset: Int): Unit = {
    val x = this.x + xOffset
    val y = this.y + yOffset
    secondaryImage.foreach(_.scaledImage(factor).drawCenteredImage(gc, x, y, side, side))
  }

  def darkeningShouldBeRedrawn: Boolean = isDarkened != currentDarkened

  def drawItself(gc: GraphicsContext, stage: HexDrawingStage, xOffset: Int, yOffset: Int) {
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
        isInterfaceDirty = false
      case ClearStage =>
        drawClearStage(gc, xOffset, yOffset)
      case ProvinceBordersStage =>
        drawProvinceBorder(gc, xOffset, yOffset)
      case BuildingsForestStage =>
        drawCastleAndWalls(gc, xOffset, yOffset)
        drawHouseIfNeeded(gc, xOffset, yOffset)
        drawForestIfNeeded(gc, xOffset, yOffset)
    }
  }


  private val anotherProvince = hex.province.map { province =>
    val neigsWithDirs = field.neighboursWithDirections(hex)
    neigsWithDirs.filter{ case (dir, n) =>
      n.province.exists(_ != province) && n.terrain.isNot(WaterKind)
    }
  }.getOrElse(Map())

  private def calculateSameOwnerBorders():List[Direction] = {
    anotherProvince.filter { case (_ , h) =>
      h.province.get.owner == hex.province.get.owner
    }.keys.toList
  }

  private def calculateDifferentOwnerBorders():List[Direction] = {
    anotherProvince.filter { case (_, h) =>
      h.province.get.owner != hex.province.get.owner
    }.keys.toList
  }

  private var sameOwnerBorders = calculateSameOwnerBorders()
  private var differentOwnerBorders = calculateDifferentOwnerBorders()

  private def drawProvinceBorder(gc: GraphicsContext, xOffset: Int, yOffset: Int): Unit = {
      def drawLine(map: List[Direction], color:Color, lineWidth: Int): Unit = {
        map.foreach{ direction =>
          val line = TerrainHexView.pointsWithDirections(side, lineWidth / 2)(direction)
          gc.save()
          gc.stroke = color
          gc.lineWidth = lineWidth
          gc.strokeLine(this.x + xOffset + line.beginX, line.beginY + this.y + yOffset, line.endX + this.x + xOffset, line.endY + this.y + yOffset)
          gc.restore()
        }
      }

    hex.province.foreach {province =>
      if (hex.terrain.isNot(WaterKind)) {
        if (provinceSelected) {
          drawLine(sameOwnerBorders, Color.Black, 4)
          drawLine(differentOwnerBorders, province.owner.color, 8)
        } else {
          drawLine(sameOwnerBorders, Color.Black, 1)
          drawLine(differentOwnerBorders, province.owner.color, 4)
        }
      }
    }
  }

  private def drawClearStage(gc: GraphicsContext, xOffset: Int, yOffset: Int) {
    gc.clearRect(x + xOffset, y + yOffset, side, side)
  }

  private def drawMovementImpossibleIfNeeded(gc: GraphicsContext, xOffset: Int, yOffset: Int) {
    if (isDarkened && arrowEnd.isEmpty) {
      gc.drawImage(TerrainHexView.movementImpossibleCache(side), x + xOffset, y + yOffset)
    }
    currentDarkened = isDarkened
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
case object BuildingsForestStage extends HexDrawingStage
case object EndInterfaceDrawing extends HexDrawingStage
case object ProvinceBordersStage extends HexDrawingStage
