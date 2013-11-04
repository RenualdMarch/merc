package mr.merc.map.pathfind

import mr.merc.map.Grid
import scala.collection.mutable.ArrayBuffer

object AStarPathFinder {
	def findPath[T](grid:Grid[T], from:T, to:T, maxPrice:Int, alreadyMoved:Boolean = false):Option[List[T]] = {
	  if (grid.cellWhereItIsForbiddenToStop(to)) {
	    return None
	  }
	  
	  val queue = collection.mutable.Queue[Node[T]]()
	  val calculated = collection.mutable.Map[T, Node[T]]()
	  queue enqueue new Node(from, 0, None)
	  
	  while (!queue.isEmpty) {
		val current = queue.dequeue()
		var addNeigboursToQueue = false
		if (current.pathPrice <= maxPrice) {
		  if (calculated.contains(current.t)) {
		      val earlierValue = calculated(current.t)
		      if (earlierValue.pathPrice > current.pathPrice) {
		        calculated.remove(earlierValue.t)
		        calculated += (current.t -> current)
		        addNeigboursToQueue = true
		      }
		    } else {
		      calculated += (current.t -> current)
		      addNeigboursToQueue = true
		    }
		  }
		  
		  val stopHere = if (from == current.t) {
		    alreadyMoved && grid.cellWhereMovementMustBeStopped(current.t)
		  } else {
		    grid.cellWhereMovementMustBeStopped(current.t)
		  }
		  
		  addNeigboursToQueue &&= !stopHere
		  
		  if (addNeigboursToQueue) {
		    queue ++= getNeighbours(current, grid)
		  }		  
		}
	  
	    calculated.get(to).map(_.path.map(_.t))
	  }
	
	private def getNeighbours[T](parent:Node[T], grid:Grid[T]):Set[Node[T]] = {
	  grid.neighbours(parent.t) filterNot(grid.isBlocked) map (n => {
	    new Node(n, grid.price(n), Some(parent))
	  })
	}
	  	
    private class Node[T](val t:T, val selfPrice:Int, var parent:Option[Node[T]]) {
     
     def path:List[Node[T]] = {
       var result = ArrayBuffer(this)
       var current = parent
       while (current.isDefined) {
         val next = current.get
         result += next
         current = next.parent
       }
       
       result.toList.reverse
     } 
      
     def pathPrice = path.map(_.selfPrice).sum
     
     override def equals(any:Any):Boolean = {
      any match {
    	case node:Node[T] => node.t == t
    	case _ => false
      }
    }
  
    override def hashCode() = t.hashCode
  }	  
}
	


