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

// TODO add update method which handles case when soldiers changed
class MapView(field: TerrainHexField, val soldiersDrawer: SoldiersDrawer = new SoldiersDrawer()) {
  val terrainView = new TerrainHexFieldView(field)

  lazy val mapImage: Image = {
    val canvas = new Canvas
    canvas.width.value = pixelWidth
    canvas.height.value = pixelHeight
    val image = new WritableImage(pixelWidth, pixelHeight)
    val gc = canvas.graphicsContext2D
    terrainView.drawItself(gc)
    val params = new SnapshotParameters
    params.fill = Color.BLACK
    canvas.snapshot(params, image)
  }
  createSoldiers foreach (soldiersDrawer.addSoldier)

  def hexByPixel(x: Int, y: Int) = terrainView.hexByPixelCoords(x, y)

  def soldiers = soldiersDrawer.soldiers

  private def createSoldiers: List[SoldierView] = {
    val soldiers = terrainView.hexes.flatMap(h => {
      val soldierOption = h.hex.soldier
      soldierOption match {
        case Some(soldier) => {
          val view = new SoldierView(soldier)
          view.x = h.x
          view.y = h.y
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

  def drawItself(gc: GraphicsContext) {
    gc.drawImage(mapImage, 0, 0)
    soldiersDrawer.drawSoldiers(gc)
    terrainView.drawMovementImpossible(gc)
    terrainView.drawDefence(gc)
    terrainView.drawArrow(gc)
  }

  def addMovement(movement: Movement) {
    soldiersDrawer.addMovement(movement)
  }

  def pixelWidth = terrainView.pixelWidth
  def pixelHeight = terrainView.pixelHeight
}