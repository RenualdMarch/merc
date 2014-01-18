package mr.merc.view.move

import mr.merc.map.hex.view.TerrainHexView

class MovementList(val list: List[Movement]) extends Movement {
  private val moves = list.toVector
  private var current = 0
  private var prevDirtyHexes: List[TerrainHexView] = Nil
  private def incrementCurrent() {
    prevDirtyHexes :::= currentMovement.dirtyHexes
    current += 1
  }

  def currentMovement = {
    try {
      moves(current)
    } catch {
      case ex: Exception => {
        println(this.isOver)
        throw ex
      }
    }
  }

  override def start() {
    super.start()
    if (!isOver) {
      callStartWhileNotIsOver()
    }
  }

  private def callStartWhileNotIsOver() {
    if (!currentMovement.isStarted) {
      currentMovement.start()
    }

    while (!isOver && currentMovement.isOver) {
      incrementCurrent()
      if (!isOver) {
        currentMovement.start()
      }
    }
  }

  override def isOver = current == list.size

  override def drawables = currentMovement.drawables

  override def dirtyHexes = if (isOver) {
    prevDirtyHexes
  } else {
    currentMovement.dirtyHexes
  }

  override def update(time: Int) {
    super.update(time)
    prevDirtyHexes = Nil

    callStartWhileNotIsOver()
    currentMovement.update(time)

    if (currentMovement.isOver) {
      incrementCurrent()
      if (!isOver) {
        callStartWhileNotIsOver()
      }
    }
  }
}