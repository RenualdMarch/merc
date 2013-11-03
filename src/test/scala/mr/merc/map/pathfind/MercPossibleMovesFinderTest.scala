package mr.merc.map.pathfind

import org.scalatest.FunSuite
import mr.merc.map.hex.HexField
import mr.merc.map.hex.Hex

class MercPossibleMovesFinderTest extends FunSuite {
	val finder = MercPossibleMovesFinder
  
    test("sanity check") {
	  val grid = new HexField[Hex](5, 5, Hex.hexInit)
      val from = grid.hex(0, 1)
	  val result = finder.findPossibleMoves(grid, from, 2, false)
	  import grid.hex
	  assert(result === Set(hex(0, 1), hex(0, 0), hex(1, 0), hex(1, 1), hex(0, 2),
	      hex(2,0), hex(2,1), hex(2, 2), hex(1, 2), hex(0, 3)))
	}
	
	test("when longest way is more effective") {
	  val grid = new HexField[Hex](5, 5, Hex.hexInit) {
	    override def price(h:Hex) = if (h.x == 0 && h.y == 1) 1000 else 1
	  }
      val from = grid.hex(0, 0)
      val result = finder.findPossibleMoves(grid, from, 3, false)
      import grid.hex
	  assert(result === Set(hex(0, 0), hex(1, 0), hex(2, 0), hex(2, 1),
	      hex(1, 1), hex(3,0), hex(3, 1), hex(2, 2), hex(1, 2), hex(0, 2)))
	}
	
	test("when we need to stop") {
	  val grid = new HexField[Hex](5, 5, Hex.hexInit) {
	    override def cellWhereMovementMustBeStopped(h:Hex) = h.x == 0 && h.y == 1
	  }
	  
      val from = grid.hex(0, 0)
	  val result = finder.findPossibleMoves(grid, from, 3, false)
      import grid.hex
	  assert(result === Set(hex(0, 0), hex(1, 0), hex(0, 1), hex(2, 0), hex(2, 1),
	      hex(1, 1), hex(3,0), hex(3, 1), hex(2, 2), hex(1, 2), hex(0, 2)))
	}
}