package mr.merc.map.hex

import scala.math.Ordering

// TODO refactor this 
object Directions extends Enumeration {
    type Direction = Value
    type DirPair = (Direction, Direction)
    val N, NE, SE, S, SW, NW = Value
    
    val list = List(N, NE, SE, S, SW, NW)
    
    def opposite(dir:Direction) = next(next(next(dir)))
    
    def next(dir:Direction):Direction = {
      if (dir != NW) {
         val index = list.indexOf(dir)
         list(index + 1)
      } else {
        N
      }
    }
    
    def prev(dir:Direction):Direction = {
      if (dir != N) {
         val index = list.indexOf(dir)
         list(index - 1)
      } else {
        NW
      }
    }
    
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
    
    //FIXME do something with hacks
    def leftSliceContainsRightSlice(left:DirPair, right:DirPair):Boolean = {
      var pair = (toDegrees(left), toDegrees(right))
      var normLeft = pair._1
      var normRight = pair._2
      
      val firstResult = normLeft._1 <= normRight._1 && normLeft._2 >= normRight._2
      if (firstResult) {
        return firstResult
      }
      
      pair = increaseSmaller(normLeft, normRight)
      normLeft = pair._1
      normRight = pair._2
      
      val secondResult = normLeft._1 <= normRight._1 && normLeft._2 >= normRight._2
      secondResult
    }
    
    //FIXME do something with hacks
    def overlapping(left:DirPair, right:DirPair):Boolean = {
      var pair = (toDegrees(left), toDegrees(right))
      var normLeft = pair._1
      var normRight = pair._2
      val firstResult = !(normRight._1 > normLeft._2 || normLeft._1 > normRight._2)
      if (firstResult) {
        return firstResult
      }
      
      pair = increaseSmaller(normLeft, normRight)
      normLeft = pair._1
      normRight = pair._2
      
      val secondResult = !(normRight._1 > normLeft._2 || normLeft._1 > normRight._2)
      secondResult
    }    
    
    private def increaseSmaller(first:(Int, Int), second:(Int, Int)):((Int, Int),(Int, Int)) = {
      if (first._1 > second._2) {
        (first, (second._1 + 360, second._2 + 360))
      } else if (second._1 > first._2) {
        ((first._1 + 360, first._2 + 360), second)
      } else {
        (first, second)
      }
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
  
    def isNeighbour(first:Direction, second:Direction) = {
      neighbours(first).contains(second)
    }
  
    def unite(first:DirPair, second:DirPair):DirPair = {
	    require(!overlapping(first, second), s"overlapping pair: $first, $second")
        require(isNeighbour(first._1, second._2) || isNeighbour(second._1, first._2))
	    
        val pair = normalizeForNieghboring(first, second)
        val firstDeg = pair._1
        val secondDeg = pair._2
        
	    if (firstDeg._1 - secondDeg._2 == 60) {
	      (toDirection(secondDeg._1), toDirection(firstDeg._2)) 
	    } else if (secondDeg._1 - firstDeg._2 == 60) {
	      (toDirection(firstDeg._1), toDirection(secondDeg._2))
	    } else {
	      throw new IllegalArgumentException(s"Impossible case: $firstDeg, $secondDeg");
	    }
    }
    
    private def normalizeForNieghboring(first:DirPair, second:DirPair):((Int, Int), (Int, Int)) = {
        var firstDeg = toDegrees(first)
        var secondDeg = toDegrees(second)
        
        if (firstDeg._1 - secondDeg._2 == 60 || secondDeg._1 - firstDeg._2 == 60) {
          return (firstDeg, secondDeg)
        }
        
        if (firstDeg._1 > secondDeg._2) {
          secondDeg = (secondDeg._1 + 360, secondDeg._2 + 360)
        } else if (secondDeg._1 > firstDeg._2) {
          firstDeg = (firstDeg._1 + 360, firstDeg._2 + 360)
        }
        (firstDeg, secondDeg)
    }
    
    def length(dir:DirPair):Int = {
      val norm = toDegrees(dir._1, dir._2)
      (norm._2 - norm._1) / 60 + 1   
    }
    
    private def toDegrees(pair:DirPair):(Int, Int) = {
      val degrees = (list.indexOf(pair._1) * 60, list.indexOf(pair._2) * 60)
      
      if (degrees._1 > degrees._2) {
        (degrees._1, degrees._2 + 360)
      } else {
        degrees
      }
    }
    
    private def toDirections(pair:(Int, Int)):DirPair = {
      if (pair._2 > 360) {
        (toDirection(pair._2), toDirection(pair._1))
      } else {
        (toDirection(pair._1), toDirection(pair._2))
      }
    }
        
    private def toDirection(degree:Int):Direction = {
      val number = degree % 360 / 60
      list(number)
    }
}