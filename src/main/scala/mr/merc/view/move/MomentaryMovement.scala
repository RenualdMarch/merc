package mr.merc.view.move


object MomentaryMovement {
  def apply(f: => Unit) = new MomentaryMovement(f)
}

class MomentaryMovement(f: => Unit) extends Movement {
  private var _isOver = false
  override def start() {
    super.start()
    f
    _isOver = true
  }
  override def isOver = _isOver

  override def update(time: Int) =
    throw new IllegalStateException("This movement doesn't support update")
}