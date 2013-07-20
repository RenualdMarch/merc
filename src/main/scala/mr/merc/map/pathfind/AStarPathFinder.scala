package mr.merc.map.pathfind

import mr.merc.map.Grid
import scala.collection.mutable.ArrayBuffer

class AStarPathFinder {
	def findPath[T](grid:Grid[T], from:T, to:T):Option[List[T]] = {
	  val openList = collection.mutable.Set[Node[T]]()
	  val closedList = collection.mutable.Set[Node[T]]()
	  
	  var found = false
	  var noRoute = false
	  
	  openList += (new Node(from))
	  while (!found && !noRoute) {
	    val smallestF = openList.reduce((x1, x2) => if (x1.f < x2.f) x1 else x2)
	    closedList += smallestF
	    openList-=(smallestF)
	    
	    val neighbours = grid.neighbours(smallestF.t).map(new Node(_))
	    neighbours.foreach(n => {
	      if (!grid.isBlocked(n.t) && !closedList.contains(n)) {
	        
	        if (!openList.contains(n)) {
	          openList += n	          
	          n.parent = smallestF
	          n.h = grid.distance(n.t, to)
              n.g = grid.price(n.t);
	        } else {	          
	          if (n.g < smallestF.g + grid.price(smallestF.t)) {
	        	  n.parent = smallestF.parent
	        	  n.h = grid.distance(n.t, to)
	        	  n.g = grid.price(smallestF.t)	           
	          }
	        }
	      }
	    })
	    
	    if (openList.contains(new Node(to))) {
           found = true;
        }

        if (openList.isEmpty) {
            noRoute = true;
        }
	  }
	  
	  if (noRoute) {
	    None
	  } else {
	    var currentNode = openList.find(_.t == to).get
	    var result = List[T]()
	    while (currentNode.t != from) {
	      result = currentNode.t :: result
	      currentNode = currentNode.parent
	    }
	    
	    result = from :: result
	    Some(result)
	  }
	}
}

private class Node[T](val t:T) {
  def f = g + h
  var g = 0
  var h = 0
  var parent = this
  
  override def equals(any:Any):Boolean = {
    any match {
    	case node:Node[T] => node.t == t
    	case _ => false
    }
  }
  
  override def hashCode() = t.hashCode
  
}