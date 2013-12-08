package mr.merc.view.move

import mr.merc.view.Drawable

trait Movement {
  var isStarted = false
  def start() {
    require(!isStarted, "Movement is already started!")
    isStarted = true
  }
  def update(time: Int) {
    require(isStarted, "Movement is not started!")
    require(!isOver, "Movement is already over!")
  }
  def isOver: Boolean
  // who is first should be rendered first
  def drawables: List[Drawable] = Nil
}