package mr.merc.map.pathfind

import mr.merc.map.Grid

object MercPossibleMovesFinder {
	def findPossibleMoves[T](grid:Grid[T], from:T, maxPrice:Int, alreadyMoved:Boolean):Set[T] = {
		val queue = collection.mutable.Queue[Node[T]]()
		val startingNode = new Node(from, 0)
		queue enqueue startingNode
		val calculated = collection.mutable.Map[Node[T], Node[T]]()
		while (!queue.isEmpty) {
		  val current = queue.dequeue()
		  var addNeigboursToQueue = false
		  if (current.pathPrice <= maxPrice) {
		    if (calculated.contains(current)) {
		        val earlierValue = calculated(current)
		        if (earlierValue.pathPrice > current.pathPrice) {
		          calculated.remove(earlierValue)
		          calculated += (current -> current)
		          addNeigboursToQueue = true
		        }
		      } else {
		        calculated += (current -> current)
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
		
		calculated.keySet.map(_.t).toSet
	}
	
	private def getNeighbours[T](parent:Node[T], grid:Grid[T]):Set[Node[T]] = {
	  grid.neighbours(parent.t) filterNot(grid.isBlocked) map (n => {
	    val node = new Node(n, grid.price(n))
	    node.pathPrice = parent.pathPrice + node.selfPrice
	    node
	  })
	}
	
	private class Node[T](val t:T, val selfPrice:Int) {
	  var pathPrice = 0
	  override def equals(any:Any) = {
	    any match {
	     	case node:Node[T] => node.t == t
	       	case _ => false
	    }
	  }
	  
	  override def hashCode = t.hashCode
	}
}