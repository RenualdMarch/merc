package mr.merc.map.hex.view

import scalafx.scene.canvas.GraphicsContext
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.CubeHex
import mr.merc.map.hex.Hex
import mr.merc.map.hex.InfiniteHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import mr.merc.unit.SoldierDefence
import scalafx.scene.image.Image
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.WritableImage
import scalafx.scene.SnapshotParameters
import scalafx.scene.paint.Color
import scalafx.geometry.Rectangle2D
import mr.merc.map.hex.Direction
import mr.merc.map.view.SoldiersDrawer
import mr.merc.ui.common.CanvasLayer
import mr.merc.map.world.WorldMap
import mr.merc.ui.common.geom.Line
import mr.merc.ui.common.geom.Polygon
import mr.merc.ui.common.geom.PolygonSet

class TerrainHexFieldView(field: TerrainHexField, soldiersDrawer: SoldiersDrawer[_], worldMap: Option[WorldMap] = None) {
  private val infiniteField = new InfiniteHexField((x, y) => new TerrainHex(x, y, Empty))

  val realHexes = field.hexes.map(new TerrainHexView(_, field, this, worldMap))
  def blackHexes: Set[TerrainHexView] = {
    val hexSet = field.hexes.toSet
    val blackHexes = hexSet.flatMap(h => infiniteField.neighbours(h.x, h.y)) -- hexSet
    blackHexes.filterNot { h =>
      (h.y == -1 && h.x % 2 == 0) ||
        (h.y == field.height && h.x % 2 == 0)
    }.map(new TerrainHexView(_, field, this))
  }

  // sorting is make sure that hexes are drawn in correct order, back before first
  val hexesToDraw = (realHexes ++ blackHexes).toList.sortBy(h => (h.hex.terrain.layer, h.hex.y, h.hex.x))
  val hexesToDrawMap = hexesToDraw.map(h => ((h.hex.x, h.hex.y), h)).toMap

  private var _movementOptions: Option[Set[TerrainHexView]] = None
  def movementOptions = _movementOptions
  def movementOptions_=(viewsOpt: Option[Set[TerrainHexView]]) {
    def darkHexes(opts: Set[TerrainHexView]) = realHexes.toSet -- opts -- arrowHexes

    _movementOptions.foreach(darkHexes(_).foreach(_.isDarkened = false))

    viewsOpt match {
      case Some(views) => darkHexes(views).foreach(_.isDarkened = true)
      case None => // do nothing
    }
    _movementOptions = viewsOpt
  }

  def neighbours(view: TerrainHexView): Set[TerrainHexView] = {
    infiniteField.neighbours(view.hex).map(n => (n.x, n.y)).collect(hexesToDrawMap)
  }

  def neighboursOfNeighbours(view: TerrainHexView): Set[TerrainHexView] = {
    neighbours(view).flatMap(neighbours)
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

  var worldMapArrows: List[(TerrainHexView, TerrainHexView)] = Nil

  private val realHexesMap = realHexes map (h => ((h.hex.x, h.hex.y), h)) toMap

  def hex(hex: Hex): TerrainHexView = this.hex(hex.x, hex.y)
  def hex(x: Int, y: Int) = realHexesMap(x, y)

  def calculateVisibleHexes(viewRect: Rectangle2D) = hexesToDraw filter (isVisible(viewRect))

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
      List(TerrainImageStage, HexGridStage).foreach { stage =>
        visibleHexes foreach (_.drawItself(gc, stage, -viewRect.minX.toInt, -viewRect.minY.toInt))
      }
    }
  }

  private val terrainWorldLayer = new CanvasLayer {
    def updateLayer(gc: GraphicsContext, viewRect: Rectangle2D) {
      // No changes possible
    }

    def drawLayer(gc: GraphicsContext, viewRect: Rectangle2D) {
      val visibleHexes = calculateVisibleHexes(viewRect)
      List(TerrainImageStage, CityDrawingStage, BordersGridStage).foreach { stage =>
        visibleHexes foreach (_.drawItself(gc, stage, -viewRect.minX.toInt, -viewRect.minY.toInt))
      }
    }
  }

  private val worldArrowsLayer = new CanvasLayer {
    val arrowColor = Color.RED

    private var currentlyDrawed: List[(TerrainHexView, TerrainHexView)] = Nil

    def updateLayer(gc: GraphicsContext, viewRect: Rectangle2D) {
      if (currentlyDrawed != worldMapArrows) {
        drawLayer(gc, viewRect)
      }
    }

    def drawLayer(gc: GraphicsContext, viewRect: Rectangle2D) {
      if (currentlyDrawed.nonEmpty) {
        gc.clearRect(0, 0, viewRect.width, viewRect.height)
      }

      currentlyDrawed = worldMapArrows
      drawArrows(gc, currentlyDrawed, viewRect)
    }

    def drawArrows(gc: GraphicsContext, arrows: List[(TerrainHexView, TerrainHexView)], viewRect: Rectangle2D) {
      arrows foreach {
        case (start, finish) =>
          val xOffset = -viewRect.minX.toInt
          val yOffset = -viewRect.minY.toInt
          arrowPolygon(start, finish).drawPolygons(xOffset, yOffset, gc, Color.RED)
      }
    }
  }

  private val interfaceBattleLayer = new CanvasLayer {
    def updateLayer(gc: GraphicsContext, viewRect: Rectangle2D) {
      val dirty = calculateVisibleHexes(viewRect).filter(_.isInterfaceDirty)
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

  def canvasBattleLayers = List(terrainBattleLayer, soldierBattleLayer, darkeningBattleLayer, interfaceBattleLayer)
  def canvasWorldLayers = List(terrainWorldLayer, soldierBattleLayer, worldArrowsLayer)

  def neigsToRedrawFromCache(set: Set[TerrainHexView]): Set[TerrainHexView] = {
    set flatMap redrawNeigsCache
  }

  val redrawNeigsCache: Map[TerrainHexView, Set[TerrainHexView]] = {
    hexesToDraw.map { h =>
      (h -> hexesToRedrawWithNeighboursOfUpperLayer(Set(h)))
    } toMap
  }

  def hexesToRedrawWithNeighboursOfUpperLayer(set: Set[TerrainHexView]): Set[TerrainHexView] = {
    val alreadyCalculated = collection.mutable.Set[TerrainHexView]()
    var lastStep = set
    while (lastStep.nonEmpty) {
      val newSteps = lastStep.flatMap(neighboursToBeRedrawn)
      alreadyCalculated ++= lastStep
      lastStep = newSteps -- alreadyCalculated
    }

    alreadyCalculated.toSet
  }

  def neighboursToBeRedrawn(view: TerrainHexView): Set[TerrainHexView] = {
    val neigs = neighbours(view)
    if (view.hex.terrain.layer == 0) {
      neigs.filter(_.hex.terrain.layer > 0)
    } else {
      val neigsSameLayer = neigs.filter(_.hex.terrain.layer == view.hex.terrain.layer)
      val neigsSameLayerToBeRepainted = neigsSameLayer.filter(n => n.hex.x > view.hex.x || n.hex.y > view.hex.x)
      val neigsOfBiggerLayer = neigs.filter(_.hex.terrain.layer < view.hex.terrain.layer)
      neigsOfBiggerLayer ++ neigsSameLayerToBeRepainted
    }
  }

  def isVisible(viewPort: Rectangle2D)(hex: TerrainHexView): Boolean = {
    val hexRect = new Rectangle2D(hex.x, hex.y, TerrainHexView.Side, TerrainHexView.Side)
    viewPort.intersects(hexRect) || viewPort.contains(hexRect)
  }

  def hexByPixelCoords(pixelX: Int, pixelY: Int): Option[TerrainHexView] = {
    val side = TerrainHexView.Side
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
    val candidates = neighbours + hex map (h => new TerrainHexView(h, field, this))
    def distanceSquare(x: Int, y: Int) = (x - pixelX) * (x - pixelX) + (y - pixelY) * (y - pixelY)
    val min = candidates.minBy(c => distanceSquare(c.center._1, c.center._2))

    realHexesMap.get(min.hex.x, min.hex.y)
  }

  def pixelWidth = hex(field.width - 1, 0).x + TerrainHexView.Side
  def pixelHeight = hex(0, field.height - 1).y + TerrainHexView.Side

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
    val line = new Line(start.center._1, start.center._2, finish.center._1, finish.center._2)
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
    val line = new Line(start.center._1, start.center._2, finish.center._1, finish.center._2)
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
}