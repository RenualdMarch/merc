package mr.merc.map.hex

object Direction {
  val list = List(N, NE, SE, S, SW, NW)
  type DirPair = (Direction, Direction)
  
  def name(n:String) = list.find(_.name.equalsIgnoreCase(n)).get
  
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
	   first._1.isNeighbour(second._2) || first._2.isNeighbour(second._1)
	}
	
	
    def unite(first:DirPair, second:DirPair):DirPair = {
	    require(!overlapping(first, second), s"overlapping pair: $first, $second")
        require(first._1.isNeighbour(second._2) || second._1.isNeighbour(first._2))
	    
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

sealed abstract class Direction(val name:String) {
    
    override def toString = name
  
    def opposite = next.next.next
    
    def next:Direction = {
      if (this != NW) {
         val index = Direction.list.indexOf(this)
         Direction.list(index + 1)
      } else {
        N
      }
    }
    
    def prev:Direction = {
      if (this != N) {
         val index = Direction.list.indexOf(this)
         Direction.list(index - 1)
      } else {
        NW
      }
    }
    
    def neighbours:Set[Direction] = {
      val index = Direction.list.indexOf(this)
      if (index == 0) {
        Set(Direction.list(1), Direction.list.last)
      } else if (index == Direction.list.size - 1) {
        Set(Direction.list(0), Direction.list(Direction.list.size - 2))
      } else {
        Set(Direction.list(index - 1), Direction.list(index + 1))
      }
    }
    
    def isNeighbour(dir:Direction) = {
      neighbours.contains(dir)
    }
}

object N extends Direction("N")
object NE extends Direction("NE")
object SE extends Direction("SE")
object S extends Direction("S")
object SW extends Direction("SW")
object NW extends Direction("NW")