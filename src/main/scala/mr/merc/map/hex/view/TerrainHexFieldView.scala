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

class TerrainHexFieldView(field: TerrainHexField, soldiersDrawer: SoldiersDrawer, worldMap: Option[WorldMap] = None) {
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

  private val realHexesMap = realHexes map (h => ((h.hex.x, h.hex.y), h)) toMap

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
  def canvasWorldLayers = List(terrainWorldLayer)

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
}