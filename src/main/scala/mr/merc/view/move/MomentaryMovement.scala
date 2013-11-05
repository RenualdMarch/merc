package mr.merc.view.move

class MomentaryMovement[T](f: => T) extends Movement {
    var isOver = false
    def start() {
      f
      isOver = true
    }
    def update(time:Int) = 
      throw new IllegalStateException("This movement doesn't support update")
}