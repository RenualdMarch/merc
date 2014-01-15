package mr.merc.view.move

import mr.merc.map.hex.view.TerrainHexView
import mr.merc.unit.view.SoldierView
import mr.merc.unit.view.MoveState
import mr.merc.unit.sound.MovementSound
import mr.merc.sound.Sound
import mr.merc.map.hex.view.TerrainHexFieldView

class SoldierMoveMovement(val from: TerrainHexView, val to: TerrainHexView, val soldier: SoldierView, fieldView: TerrainHexFieldView) extends Movement {
  private val movementSpeed = 100
  private val linearMovement = new LinearMovement(from.x, from.y, to.x, to.y, movementSpeed)

  override def start() {
    super.start()
    from.soldier = None
    soldier.state = MoveState
    linearMovement.start()
    updateSoldierCoords()
    soldier.sounds.get(MovementSound).foreach(_.play)
  }

  private def updateSoldierCoords() {
    soldier.x = linearMovement.x
    soldier.y = linearMovement.y
  }

  override def update(time: Int) {
    super.update(time)
    linearMovement.update(time)
    updateSoldierCoords()
    if (isOver) {
      to.soldier = Some(soldier)
    }
  }

  def isOver = linearMovement.isOver

  override def drawables = List(soldier)

  override def dirtyHexes = (fieldView.neighbours(from) | fieldView.neighbours(to)) toList
}