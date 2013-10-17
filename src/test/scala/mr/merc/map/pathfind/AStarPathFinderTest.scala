package mr.merc.map.pathfind

import org.scalatest.FunSuite
import mr.merc.map.hex.Hex
import mr.merc.map.hex.HexField
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain.Grass

class AStarPathFinderTest extends FunSuite{
	val finder = AStarPathFinder
  
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
	
	test("pathfinding with cells where you can only to stop") {
	  val grid = new HexField[Hex](5, 5, Hex.hexInit) {
	    override def cellWhereMovementMustBeStopped(h:Hex) = {
	      h.x == 1 && h.y == 0 || h.x == 2 && h.y == 1
	    }
	  }
	  
	  val from = grid.hex(0, 0)
	  val dest = grid.hex(2, 1)
	  val result = finder.findPath(grid, from, dest)
	  val connector1 = grid.hex(0, 1)
	  val connector2 = grid.hex(1, 1)
	  assert(result.get === List(from, connector1, connector2, dest))
	}
	
	test("pathfinding with cells that are forbidden to stop") {
	  val grid = new HexField[Hex](5, 5, Hex.hexInit) {
	    override def cellWhereItIsForbiddenToStop(h:Hex) = {
	      h.x == 1 && h.y == 0
	    }
	  }
	  
	  val from = grid.hex(0, 0)
	  val dest = grid.hex(1, 0)
	  val result = finder.findPath(grid, from, dest)
	  assert(result === None)
	}
	
	test("simple pathfinding") {
	  val grid = new TerrainHexField(10, 10, (x, y) => new TerrainHex(x, y, Grass))
	  val from = grid.hex(0, 0)
	  val dest = grid.hex(5, 2)
	  val result = finder.findPath(grid, from, dest)
	  import grid.hex
	  assert(result.get === List(hex(0, 0), hex(1, 0), hex(2, 1), hex(3, 1), hex(4, 2), hex(5, 2)))
	}
}