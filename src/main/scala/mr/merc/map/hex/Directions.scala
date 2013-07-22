package mr.merc.map.hex

object Directions extends Enumeration {
    type Direction = Value
    val N, S, NE, SE, NW, SW = Value
    
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
        
    def normalizeClockwise(from:Direction , to:Direction):(Direction, Direction) = {
      val fromIndex = list.indexOf(from)
      val toIndex = list.indexOf(to)
      if (fromIndex < toIndex) {
        (from, to)
      } else {
        (to, from)
      }
    }
}