package mr.merc.view.move

class MovementList(val list: List[Movement]) extends Movement {
  private val moves = list.toVector
  private var current = 0

  def currentMovement = moves(current)

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
      current += 1
      if (!isOver) {
        currentMovement.start()
      }
    }
  }

  override def isOver = current == list.size

  override def drawables = currentMovement.drawables

  override def update(time: Int) {
    super.update(time)

    callStartWhileNotIsOver()
    currentMovement.update(time)

    if (currentMovement.isOver) {
      current += 1
      if (!isOver) {
        callStartWhileNotIsOver()
      }
    }
  }
}