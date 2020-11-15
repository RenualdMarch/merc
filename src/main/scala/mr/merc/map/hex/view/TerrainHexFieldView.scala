package mr.merc.map.hex.view

import scalafx.scene.canvas.GraphicsContext
import mr.merc.map.hex._
import mr.merc.map.terrain._
import mr.merc.unit.SoldierDefence
import scalafx.geometry.Rectangle2D
import mr.merc.map.view.SoldiersDrawer
import mr.merc.ui.common.CanvasLayer
import mr.merc.ui.common.geom.Line
import mr.merc.ui.common.geom.Polygon
import mr.merc.ui.common.geom.PolygonSet
import mr.merc.log.Logging
import mr.merc.map.hex.view.TerrainHexFieldView.{BattleFieldViewMode, FieldViewMode, WorldMapViewMode}
import mr.merc.map.objects.Walls
import mr.merc.map.objects.view.{WallImage, WallView}
import mr.merc.unit.view.SoldierView

import scala.collection.mutable

class TerrainHexFieldView(field: TerrainHexField, val soldiersDrawer: SoldiersDrawer[SoldierView], factor: Double, mode: FieldViewMode = BattleFieldViewMode) extends Logging {
  private val infiniteField = new InfiniteHexField((x, y) => new TerrainHex(x, y, Empty))

  val realHexes = field.hexes.map(new TerrainHexView(_, field, this, factor))

  def blackHexes: Set[TerrainHexView] = {
    val hexSet = field.hexes.toSet
    val blackHexes = hexSet.flatMap(h => infiniteField.neighbours(h.x, h.y)) -- hexSet
    blackHexes.filterNot { h =>
      (h.y == -1 && h.x % 2 == 0) ||
        (h.y == field.height && h.x % 2 == 0)
    }.map(new TerrainHexView(_, field, this, factor))
  }

  private val walls: Walls = realHexes.filter(_.hex.terrain == Castle).foldLeft(Walls()) {
    case (w, hexView) =>
      w.addWall(hexView.hex)
  }
  private val wallsView = new WallView(walls)
  private val castleImages = wallsView.wallImages()

  def castleImagesForHex(hex: Hex): List[WallImage] = castleImages.getOrElse(hex, Nil)

  // sorting is make sure that hexes are drawn in correct order, back before first
  def hexSortingKey(h: TerrainHexView): (Double, Int) = (h.hex.y + h.hex.x % 2 * 0.5, h.hex.x)

  val hexesToDraw = (realHexes ++ blackHexes).toList.sortBy(hexSortingKey)
  val hexesToDrawMap = mutable.LinkedHashMap() ++ hexesToDraw.map(h => ((h.hex.x, h.hex.y), h))

  private var _movementOptions: Option[Set[TerrainHexView]] = None

  def movementOptions = _movementOptions

  def movementOptions_=(viewsOpt: Option[Set[TerrainHexView]]) {
    def darkHexes(opts: Set[TerrainHexView]) = realHexes.toSet -- opts -- arrowHexes

    _movementOptions.foreach(darkHexes(_).foreach(_.darkened = false))

    viewsOpt match {
      case Some(views) => darkHexes(views).foreach(_.darkened = true)
      case None => // do nothing
    }
    _movementOptions = viewsOpt
  }

  def neighbours(view: TerrainHexView): List[TerrainHexView] = {
    infiniteField.neighbours(view.hex).map(n => (n.x, n.y)).collect(hexesToDrawMap)
  }

  def neighboursSet(view: TerrainHexView): Set[TerrainHexView] = neighbours(view).toSet

  def neighboursOfNeighbours(view: TerrainHexView): Set[TerrainHexView] = {
    neighboursSet(view).flatMap(neighbours)
  }

  def neighboursWithDirections(view: TerrainHexView): Map[Direction, TerrainHexView] = {
    infiniteField.neighboursWithDirections(view.hex).filter {
      case (d, h) => hexesToDrawMap.contains(h.x, h.y)
    } mapValues { h => hexesToDrawMap(h.x, h.y) }
  }

  private var _arrow: Option[(TerrainHexView, TerrainHexView)] = None

  def arrow = _arrow

  def arrow_=(ar: Option[(TerrainHexView, TerrainHexView)]) {
    _arrow.foreach {
      case (from, to) =>
        from.arrowStart = None
        to.arrowEnd = None
    }

    _arrow = ar
    _arrow.foreach {
      case (from, to) =>
        val direction = field.direction(from.hex, to.hex)
        from.arrowStart = Some(direction)
        to.arrowEnd = Some(direction)
    }
  }

  def arrowHexes = arrow.map(p => Set(p._1, p._2)).getOrElse(Set())

  private var _defence: Option[(TerrainHexView, SoldierDefence, Boolean)] = None

  def defence = _defence

  def defence_=(d: Option[(TerrainHexView, SoldierDefence, Boolean)]) {
    if (_defence != d) {
      _defence.foreach {
        case (hexView, defence, drawPolygon) =>
          hexView.defence = None
      }
      _defence = d
      _defence.foreach {
        case (hexView, defence, drawPolygon) =>
          hexView.defence = Some(defence, drawPolygon)
      }
    }
  }

  private val realHexesMap = realHexes map (h => ((h.hex.x, h.hex.y), h)) toMap

  def hex(hex: Hex): TerrainHexView = this.hex(hex.x, hex.y)

  def hex(x: Int, y: Int) = realHexesMap(x, y)

  private val hexSide = TerrainHexView.side(factor)

  private var prevResult: (Rectangle2D, List[TerrainHexView]) = (new Rectangle2D(0, 0, 1, 1), Nil)

  def calculateVisibleHexes(viewRect: Rectangle2D): List[TerrainHexView] = {
    if (prevResult._1 == viewRect) {
      prevResult._2
    } else {
      val x = viewRect.minX.toInt * 4 / hexSide / 3 - 1
      val y = viewRect.minY.toInt / hexSide - 1
      val xx = x + viewRect.width.toInt * 4 / hexSide / 3 + 2
      val yy = y + viewRect.height.toInt / hexSide + 2

      val list = (for { curX <- x to xx
                       curY <- y to yy
      } yield {
        hexesToDrawMap.get(curX, curY)
      }).toList.flatten.sortBy(hexSortingKey)

      prevResult = (viewRect, list)
      list
    }

  }

  private val soldierBattleLayer = new CanvasLayer {
    def updateLayer(gc: GraphicsContext, viewRect: Rectangle2D) {
      soldiersDrawer.drawSoldiers(gc, viewRect)
    }

    def drawLayer(gc: GraphicsContext, viewRect: Rectangle2D) {
      soldiersDrawer.drawSoldiersFromScratch(gc, viewRect)
    }
  }

  private val terrainBattleLayer = new CanvasLayer {
    def updateLayer(gc: GraphicsContext, viewRect: Rectangle2D) {
      // No changes possible
    }

    def drawLayer(gc: GraphicsContext, viewRect: Rectangle2D) {
      val visibleHexes = calculateVisibleHexes(viewRect)
      List(TerrainImageStage, BuildingsForestMountainsStage, HexGridStage).foreach { stage =>
        visibleHexes foreach (_.drawItself(gc, stage, -viewRect.minX.toInt, -viewRect.minY.toInt))
      }
    }
  }

  private val terrainWorldLayer = new CanvasLayer {
    def updateLayer(gc: GraphicsContext, viewRect: Rectangle2D) {
      val dirty = calculateVisibleHexes(viewRect).filter(_.terrainDirty)
      List(TerrainImageStage, BuildingsForestMountainsStage, ProvinceBordersStage, ProvinceNamesStage).foreach { stage =>
        dirty foreach (_.drawItself(gc, stage, -viewRect.minX.toInt, -viewRect.minY.toInt))
      }
      dirty.foreach(_.terrainDirty = false)
    }

    def drawLayer(gc: GraphicsContext, viewRect: Rectangle2D) {
      val visibleHexes = calculateVisibleHexes(viewRect)
      List(TerrainImageStage, BuildingsForestMountainsStage, ProvinceBordersStage, ProvinceNamesStage).foreach { stage =>
        visibleHexes foreach (_.drawItself(gc, stage, -viewRect.minX.toInt, -viewRect.minY.toInt))
      }
      visibleHexes.foreach(_.terrainDirty = false)
    }
  }

  private val battleInterfaceBattleLayer = new CanvasLayer {
    def updateLayer(gc: GraphicsContext, viewRect: Rectangle2D) {
      val dirty = calculateVisibleHexes(viewRect).filter(_.interfaceDirty)
      List(ClearStage, ArrowStage, DefenceStage, EndInterfaceDrawing).foreach { stage =>
        dirty foreach (_.drawItself(gc, stage, -viewRect.minX.toInt, -viewRect.minY.toInt))
      }
    }

    def drawLayer(gc: GraphicsContext, viewRect: Rectangle2D) {
      val visibleHexes = calculateVisibleHexes(viewRect)
      List(ClearStage, ArrowStage, DefenceStage, EndInterfaceDrawing).foreach { stage =>
        visibleHexes foreach (_.drawItself(gc, stage, -viewRect.minX.toInt, -viewRect.minY.toInt))
      }
    }
  }

  private val darkeningBattleLayer = new CanvasLayer {
    def updateLayer(gc: GraphicsContext, viewRect: Rectangle2D) {
      val visibleHexes = calculateVisibleHexes(viewRect)
      val set = if (visibleHexes.exists(_.darkeningShouldBeRedrawn)) {
        visibleHexes
      } else {
        Nil
      }
      List(ClearStage, MovementImpossibleStage).foreach { stage =>
        set foreach (_.drawItself(gc, stage, -viewRect.minX.toInt, -viewRect.minY.toInt))
      }
    }

    def drawLayer(gc: GraphicsContext, viewRect: Rectangle2D) {
      val visibleHexes = calculateVisibleHexes(viewRect)
      List(ClearStage, MovementImpossibleStage).foreach { stage =>
        visibleHexes foreach (_.drawItself(gc, stage, -viewRect.minX.toInt, -viewRect.minY.toInt))
      }
    }
  }

  def canvasLayers: List[CanvasLayer] = mode match {
    case BattleFieldViewMode => List(terrainBattleLayer, soldierBattleLayer, darkeningBattleLayer, battleInterfaceBattleLayer)
    case WorldMapViewMode => List(terrainWorldLayer, soldierBattleLayer)
  }

  def side: Int = realHexes.head.side

  def hexByPixelCoords(pixelX: Int, pixelY: Int): Option[TerrainHexView] = {
    // first part is transform hexes into squares
    // and decide to what square belongs point
    // odd rows are +36 on y
    // side is 3/4 * hex side

    val column = pixelX / (side * 3 / 4)
    val row = if (column % 2 == 0) {
      pixelY / side
    } else {
      (pixelY - side / 2) / side
    }

    val hex = infiniteField.hex(column, row)
    val neighbours = infiniteField.neighbours(hex)
    val candidates = (hex :: neighbours) map (h => new TerrainHexView(h, field, this, factor))

    def distanceSquare(x: Int, y: Int) = (x - pixelX) * (x - pixelX) + (y - pixelY) * (y - pixelY)

    val min = candidates.minBy(c => distanceSquare(c.center._1, c.center._2))

    realHexesMap.get(min.hex.x, min.hex.y)
  }

  def pixelWidth = hex(field.width - 1, 0).x + side

  def pixelHeight = hex(0, field.height - 1).y + side

  private val hexOffset = 60
  private val arrowLength = 60
  private val arrowLineWidth = 20
  private val arrowWidth = 60

  def arrowPolygon(start: TerrainHexView, finish: TerrainHexView): PolygonSet = {
    val head = arrowHead(start, finish)
    val body = arrowLine(start, finish)
    new PolygonSet(head, body)
  }

  private def arrowHead(start: TerrainHexView, finish: TerrainHexView): Polygon = {
    val line = Line(start.center._1, start.center._2, finish.center._1, finish.center._2)
    val cutLine = line.cutFromTheBeginning(hexOffset).cutFromTheEnd(hexOffset)
    val cutFromArrow = cutLine.cutFromTheBeginning(cutLine.length - arrowLength)

    val vector = cutFromArrow.toMVector
    val orth = vector.ortho
    val firstSide = orth * (arrowWidth / 2)
    val firstLine = firstSide.toLine(cutFromArrow.beginX, cutFromArrow.beginY)
    val secondSide = orth * (-arrowWidth / 2)
    val secondLine = secondSide.toLine(cutFromArrow.beginX, cutFromArrow.beginY)

    new Polygon((cutFromArrow.endX, cutFromArrow.endY), (firstLine.endX, firstLine.endY), (secondLine.endX, secondLine.endY))
  }

  private def arrowLine(start: TerrainHexView, finish: TerrainHexView): Polygon = {
    val line = Line(start.center._1, start.center._2, finish.center._1, finish.center._2)
    val cutLine = line.cutFromTheBeginning(hexOffset).cutFromTheEnd(hexOffset)
    val cutWithoutArrow = cutLine.cutFromTheEnd(arrowLength)

    val vector = cutWithoutArrow.toMVector
    val orthoNorm = vector.ortho
    val ortho1 = orthoNorm * (arrowLineWidth / 2)
    val ortho2 = ortho1 * (-1)
    val begin1 = (cutWithoutArrow.beginX + ortho1.x, cutWithoutArrow.beginY + ortho1.y)
    val begin2 = (cutWithoutArrow.beginX + ortho2.x, cutWithoutArrow.beginY + ortho2.y)
    val end1 = (cutWithoutArrow.endX + ortho1.x, cutWithoutArrow.endY + ortho1.y)
    val end2 = (cutWithoutArrow.endX + ortho2.x, cutWithoutArrow.endY + ortho2.y)

    new Polygon(begin1, begin2, end2, end1)
  }

  def setTerrainDirty(hex: TerrainHexView): Unit = {
    hex :: neighbours(hex) foreach {
      _.terrainDirty = true
    }
  }

  def refreshTerrainDirt(): Unit = {
    hexesToDraw.foreach(_.recalculateTerrainDirtIfNeeded())
  }
}

object TerrainHexFieldView {

  sealed trait FieldViewMode

  object BattleFieldViewMode extends FieldViewMode

  object WorldMapViewMode extends FieldViewMode

}