package mr.merc.view.move

import mr.merc.economics.WorldConstants
import mr.merc.map.hex.view.TerrainHexView
import mr.merc.unit.view.SoldierView
import mr.merc.unit.view.MoveState
import mr.merc.unit.sound.MovementSound
import mr.merc.sound.Sound
import mr.merc.map.hex.view.TerrainHexFieldView

class SoldierMoveMovement(val from: TerrainHexView, val to: TerrainHexView, val soldier: SoldierView, fieldView: TerrainHexFieldView) extends Movement {
  private val movementSpeed = WorldConstants.Animation.SoldierMovementSpeed
  private val linearMovement = new LinearMovement(from.x, from.y, to.x, to.y, movementSpeed)

  override def start() {
    super.start()
    soldier.state = MoveState
    linearMovement.start()
    updateSoldierCoords()
    soldier.sounds.get(MovementSound).foreach(_.play())
  }

  private def updateSoldierCoords() {
    soldier.coords = (linearMovement.x, linearMovement.y)
  }

  override def update(time: Int) {
    super.update(time)
    linearMovement.update(time)
    updateSoldierCoords()
  }

  def isOver: Boolean = linearMovement.isOver

  override def drawables = List(soldier)
}