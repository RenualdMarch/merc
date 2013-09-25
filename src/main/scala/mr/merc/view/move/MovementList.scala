package mr.merc.view.move

class MovementList(list:List[Movement]) extends Movement {
	private val moves = list.toVector
	private var current = 0
	
	override def isOver = current == list.size
	
	override def update(time:Int) {
	  if (!isOver) {
	    moves(current).update(time)
	    if (moves(current).isOver) {
	      current += 1
	    }
	  }
	}
}