package mr.merc.view.move

import mr.merc.map.hex.view.TerrainHexView
import mr.merc.unit.view.SoldierView

class SoldierMoveMovement(val from:TerrainHexView, val to:TerrainHexView, val soldier:SoldierView) extends Movement {
  private val movementSpeed = 100  
  private val linearMovement = new LinearMovement(from.x, from.y, to.x, to.y, movementSpeed)
  
  def start() {
    linearMovement.start()
    updateSoldierCoords()
  }
  
  private def updateSoldierCoords() {
    soldier.x = linearMovement.x + 36
    soldier.y = linearMovement.y + 36
  }
  
  def update(time:Int) {
    linearMovement.update(time)
    updateSoldierCoords()
  }
  
  def isOver = linearMovement.isOver
  
  override def drawables = List(soldier)
}