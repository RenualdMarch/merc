package mr.merc.view.move

class MovementList(list:List[Movement]) extends Movement {
	private val moves = list.toVector
	private var current = 0
	
	def currentMovement = moves(current)
	
	override def start(){
	   callStartWhileNotIsOver()
	}
	
	private def callStartWhileNotIsOver() {
	  currentMovement.start()
	  while(!isOver && currentMovement.isOver) {
	    current += 1
	    if (!isOver) {
	    	currentMovement.start()
	    }
	  }
	}
	
	override def isOver = current == list.size
	
	override def update(time:Int) {
	  require(!isOver, "Action already over")
	  
	  currentMovement.update(time)
	  if (currentMovement.isOver) {
	      current += 1
	      if (!isOver) {
	    	callStartWhileNotIsOver()
	      }	    
	  }
	}
}