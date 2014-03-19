package mr.merc.map.view

import mr.merc.map.hex.view.TerrainHexFieldView
import mr.merc.map.hex.TerrainHexField
import scalafx.scene.canvas.GraphicsContext
import mr.merc.unit.view.SoldierView
import mr.merc.map.hex.view.TerrainHexView
import mr.merc.view.move.Movement
import scalafx.scene.image.Image
import scalafx.scene.image.WritableImage
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.ImageView
import scalafx.scene.SnapshotParameters
import scalafx.scene.paint.Color
import scalafx.geometry.Rectangle2D
import mr.merc.log.Logging

// TODO add update method which handles case when soldiers changed
class MapView(field: TerrainHexField, val soldiersDrawer: SoldiersDrawer[SoldierView] = new SoldiersDrawer[SoldierView]()) extends Logging {
  val terrainView = new TerrainHexFieldView(field, soldiersDrawer)

  createSoldiers foreach (soldiersDrawer.addSoldier)

  def hexByPixel(x: Int, y: Int) = terrainView.hexByPixelCoords(x, y)

  def soldiers = soldiersDrawer.soldiers

  private def createSoldiers: List[SoldierView] = {
    val soldiers = terrainView.realHexes.flatMap(h => {
      val soldierOption = h.hex.soldier
      soldierOption match {
        case Some(soldier) => {
          debug(s"Creating soldier view for soldier type ${soldier.soldierType.name}")
          val view = new SoldierView(soldier)
          view.coords = (h.x, h.y)
          Some(view)
        }
        case None => None
      }

    })

    soldiers toList
  }

  def update(time: Int) {
    soldiersDrawer.update(time)
  }

  def canvasBattleLayers = terrainView.canvasBattleLayers

  def addMovement(movement: Movement) {
    soldiersDrawer.addMovement(movement)
  }

  def pixelWidth = terrainView.pixelWidth
  def pixelHeight = terrainView.pixelHeight
}