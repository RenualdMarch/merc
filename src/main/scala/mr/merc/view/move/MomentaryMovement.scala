package mr.merc.view.move

import mr.merc.map.hex.view.TerrainHexView

object MomentaryMovement {
  def apply(f: => Unit) = new MomentaryMovement(f)
}

class MomentaryMovement(f: => Unit, override val dirtyHexes: List[TerrainHexView] = Nil) extends Movement {
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