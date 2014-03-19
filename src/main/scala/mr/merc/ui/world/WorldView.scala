package mr.merc.ui.world

import mr.merc.map.hex.view.TerrainHexFieldView
import mr.merc.map.world.Province
import mr.merc.ui.common.geom.PolygonSet
import mr.merc.map.world.WorldMap
import mr.merc.map.hex.view.TerrainHexView
import scalafx.scene.canvas.GraphicsContext
import scalafx.geometry.Rectangle2D
import mr.merc.map.view.SoldiersDrawer
import mr.merc.world.view.CharacterView
import mr.merc.map.view.MapView

class WorldView(worldMap: WorldMap) {
  val soldierDrawer = new SoldiersDrawer[CharacterView]

  // TODO add initial characters

  val hexView = new TerrainHexFieldView(worldMap.hexField, soldierDrawer, Some(worldMap))
  def update(time: Int) {
    soldierDrawer.update(time)
  }

  def selectedArrow(x: Int, y: Int): Option[(TerrainHexView, TerrainHexView)] = {
    hexView.worldMapArrows.find {
      case (s, f) =>
        val polygon = hexView.arrowPolygon(s, f)
        polygon.isInside(x, y)
    }
  }

  def canvasLayers = hexView.canvasWorldLayers

  private def arrowPolygon(start: Province, finish: Province): PolygonSet = {
    val startView = hexView.hex(start.settlementHex.x, start.settlementHex.y)
    val finishView = hexView.hex(finish.settlementHex.x, finish.settlementHex.y)
    hexView.arrowPolygon(startView, finishView)
  }
}