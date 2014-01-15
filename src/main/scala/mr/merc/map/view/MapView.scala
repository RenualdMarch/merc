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

// TODO add update method which handles case when soldiers changed
class MapView(field: TerrainHexField, val soldiersDrawer: SoldiersDrawer = new SoldiersDrawer()) {
  val terrainView = new TerrainHexFieldView(field)

  createSoldiers foreach (soldiersDrawer.addSoldier)

  def hexByPixel(x: Int, y: Int) = terrainView.hexByPixelCoords(x, y)

  def soldiers = soldiersDrawer.soldiers

  private def createSoldiers: List[SoldierView] = {
    val soldiers = terrainView.realHexes.flatMap(h => {
      val soldierOption = h.hex.soldier
      soldierOption match {
        case Some(soldier) => {
          val view = new SoldierView(soldier)
          view.x = h.x
          view.y = h.y
          h.soldier = Some(view)
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

  def drawItself(gc: GraphicsContext, viewPort: Rectangle2D) {
    soldiersDrawer.dirtyHexesInMovements.foreach(_.isDirty = true)
    terrainView.drawItself(gc, viewPort, soldiersDrawer)
  }

  def addMovement(movement: Movement) {
    soldiersDrawer.addMovement(movement)
  }

  def pixelWidth = terrainView.pixelWidth
  def pixelHeight = terrainView.pixelHeight
}