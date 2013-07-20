package mr.merc.map.pathfind

import org.scalatest.FunSuite
import mr.merc.map.hex.HexField
import mr.merc.map.hex.HexField
import mr.merc.map.hex.Hex

class AStarPathFinderTest extends FunSuite{
	val finder = new AStarPathFinder()
  
	test("pathfinding sanity check") {
		val grid = new HexField(5, 5)
		val from = grid.hex(1, 0)
		val to = grid.hex(3, 0)
		val connector = grid.hex(2, 0)
		val result = finder.findPath(grid, from, to)
		assert(result.get === List(from, connector, to))
	}
	
	test ("pathfinding with blocked path") {
	  val grid = new HexField(5, 5) {
	    override def isBlocked(hex:Hex) = hex.x == 2 && hex.y == 0
	  }
	  
	  val from = grid.hex(1, 0)
	  val to = grid.hex(3, 0)
	  val connector1 = grid.hex(1, 1)
	  val connector2 = grid.hex(2, 1)
	  
	  val result = finder.findPath(grid, from, to)
	  assert(result.get === List(from, connector1, connector2, to))
	}
	
	test("pathfinding with no path") {
	   val grid = new HexField(5, 5) {
	    override def isBlocked(hex:Hex) = hex.y == 1
	  }
	   
	  val from = grid.hex(1, 0)
	  val to = grid.hex(3, 4)
	   
	  val result = finder.findPath(grid, from, to)
	  assert(result.isEmpty)
	}
}