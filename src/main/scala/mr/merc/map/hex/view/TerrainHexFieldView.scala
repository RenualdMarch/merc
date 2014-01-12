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

class TerrainHexFieldView(field: TerrainHexField) {
  private val infiniteField = new InfiniteHexField((x, y) => new TerrainHex(x, y, Empty))

  val hexes = field.hexes.map(new TerrainHexView(_, field)) ++ blackHexes
  val hexMap = hexes.map(h => ((h.hex.x, h.hex.y), h)).toMap

  def blackHexes: Set[TerrainHexView] = {
    val hexSet = field.hexes.toSet
    val blackHexes = hexSet.flatMap(h => infiniteField.neighbours(h.x, h.y)) -- hexSet
    blackHexes.filterNot { h =>
      (h.y == -1 && h.x % 2 == 0) ||
        (h.y == field.height && h.x % 2 == 0)
    }.map(new TerrainHexView(_, field))
  }

  private val hexSet = hexes toSet

  private var _movementOptions: Option[Set[TerrainHexView]] = None
  def movementOptions = _movementOptions
  def movementOptions_=(viewsOpt: Option[Set[TerrainHexView]]) {
    def darkHexes(opts: Set[TerrainHexView]) = hexSet -- opts -- arrowHexes

    _movementOptions.foreach(darkHexes(_).foreach(_.isDarkened = false))

    viewsOpt match {
      case Some(views) => darkHexes(views).foreach(_.isDarkened = true)
      case None => // do nothing
    }

    _movementOptions = viewsOpt
  }

  def neighbours(view: TerrainHexView): Set[TerrainHexView] = {
    infiniteField.neighbours(view.hex).map(n => (n.x, n.y)).map(hexMap)
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

  private val map = hexes map (h => ((h.hex.x, h.hex.y), h)) toMap

  def hex(x: Int, y: Int) = map(x, y)

  def drawItself(gc: GraphicsContext, viewPort: Rectangle2D) {
    hexes filter (isVisible(viewPort)) foreach (_.drawItself(gc))
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
    val candidates = neighbours + hex map (h => new TerrainHexView(h, field))
    def distanceSquare(x: Int, y: Int) = (x - pixelX) * (x - pixelX) + (y - pixelY) * (y - pixelY)
    val min = candidates.minBy(c => distanceSquare(c.center._1, c.center._2))

    map.get(min.hex.x, min.hex.y)
  }

  def pixelWidth = hex(field.width - 1, 0).x + TerrainHexView.Side
  def pixelHeight = hex(0, field.height - 1).y + TerrainHexView.Side
}