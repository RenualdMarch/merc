package mr.merc.map.pathfind

import org.scalatest.FunSuite
import mr.merc.map.hex.Hex
import mr.merc.map.hex.HexField

class AStarPathFinderTest extends FunSuite{
	val finder = new AStarPathFinder()
  
	test("pathfinding sanity check") {
		val grid = new HexField[Hex](5, 5, Hex.hexInit)
		val from = grid.hex(0, 1)
		val to = grid.hex(0, 3)
		val connector = grid.hex(0, 2)
		val result = finder.findPath(grid, from, to)
		assert(result.get === List(from, connector, to))
	}
	
	test ("pathfinding with blocked path") {
	  val grid = new HexField[Hex](5, 5, Hex.hexInit) {
	    override def isBlocked(hex:Hex) = hex.x == 0 && hex.y == 2
	  }
	  
	  val from = grid.hex(0, 1)
	  val to = grid.hex(0, 3)
	  val connector1 = grid.hex(1, 1)
	  val connector2 = grid.hex(1, 2)
	  
	  val result = finder.findPath(grid, from, to)
	  assert(result.get === List(from, connector1, connector2, to))
	}
	
	test("pathfinding with no path") {
	   val grid = new HexField[Hex](5, 5, Hex.hexInit) {
	    override def isBlocked(hex:Hex) = hex.x == 1
	  }
	   
	  val from = grid.hex(0, 1)
	  val to = grid.hex(4, 3)
	   
	  val result = finder.findPath(grid, from, to)
	  assert(result.isEmpty)
	}
}