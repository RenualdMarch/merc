package mr.merc.map.hex

object Direction {
  val list = List(N, NE, SE, S, SW, NW)
  type DirPair = (Direction, Direction)
  
  def name(n:String):Direction = list.find(_.name.equalsIgnoreCase(n)).get
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