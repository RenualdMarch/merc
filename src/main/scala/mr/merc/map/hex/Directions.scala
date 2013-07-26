package mr.merc.map.hex

import scala.math.Ordering

object Directions extends Enumeration {
    type Direction = Value
    type DirPair = (Direction, Direction)
    val N, NE, SE, S, SW, NW = Value
    
    val list = List(N, NE, SE, S, SW, NW)
    
    def neighbours(dir:Direction):Set[Direction] = {
      val index = list.indexOf(dir)
      if (index == 0) {
        Set(list(1), list.last)
      } else if (index == list.size - 1) {
        Set(list(0), list(list.size - 2))
      } else {
        Set(list(index - 1), list(index + 1))
      }
    }  
        
    def normalizeClockwise(from:Direction , to:Direction):DirPair = {
      val fromIndex = list.indexOf(from)
      val toIndex = list.indexOf(to)
      if (fromIndex < toIndex) {
        (from, to)
      } else {
        (to, from)
      }
    }
    
    def leftSliceContainsRightSlice(left:DirPair, right:DirPair):Boolean = {
      val normLeft = normalizeClockwise(left._1, left._2)
      val normRight = normalizeClockwise(right._1, right._2)
      normLeft._1 <= normRight._1 && normLeft._2 >= normRight._2
    }
    
    def overlapping(left:DirPair, right:DirPair):Boolean = {
      val normLeft = normalizeClockwise(left._1, left._2)
      val normRight = normalizeClockwise(right._1, right._2)
      !(normRight._1 > normLeft._2 || normLeft._1 > normRight._2)
    }
    
    def unite(set:Set[DirPair]):Set[DirPair] = {
      var retSet = set      
      while(whichCanBeUnited(retSet).isDefined) {
        val toUnite = whichCanBeUnited(retSet).get
        val united = unite(toUnite._1, toUnite._2)
        retSet -= toUnite._1
        retSet -= toUnite._2
        retSet += united
      }
      
      retSet
    }
    
    private def whichCanBeUnited(tr:Traversable[DirPair]):Option[(DirPair, DirPair)] = {
	    tr.foreach(add1 => {
	      tr.foreach(add2 => {
	        if(add1 != add2 && canBeUnited(add1, add2)) {
	          return Some((add1, add2))
	        }
	      })
	    })
	    
	    None
    } 
    
	def canBeUnited(first:DirPair, second:DirPair):Boolean = {
	   isNeighbour(first._1, second._2) || isNeighbour(first._2, second._1)
	}
  
    def isNeighbour(first:Direction, second:Direction) = neighbours(first).contains(second)
  
    def unite(first:DirPair, second:DirPair):DirPair = {
	    if (isNeighbour(first._1, second._2)) {
	      normalizeClockwise(second._1, first._2)
	    } else if (isNeighbour(second._1, first._2)) {
	      normalizeClockwise(first._1, second._2)
	    } else {
	      throw new IllegalAccessException("slices are not neighbours")
	    }
    }
    
    def length(dir:DirPair):Int = {
      val norm = normalizeClockwise(dir._1, dir._2)
      list.indexOf(dir._2) - list.indexOf(dir._1) + 1      
    }
}