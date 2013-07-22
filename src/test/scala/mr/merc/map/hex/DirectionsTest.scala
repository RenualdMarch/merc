package mr.merc.map.hex

import org.scalatest.FunSuite

class DirectionsTest extends FunSuite {
	test("in the middle") {
	  val set = Directions.neighbours(Directions.S)
	  assert(set === Set(Directions.SE, Directions.SW))
	}
	
	test("beginning") {
	  val set = Directions.neighbours(Directions.N)
	  assert(set === Set(Directions.NE, Directions.NW))
	}
	
	test("end") {
	  val set = Directions.neighbours(Directions.NW)
	  assert(set === Set(Directions.SW, Directions.N))
	}
	
	test("clockwise sort") {
	  val result1 = Directions.normalizeClockwise(Directions.N, Directions.S)
	  assert(result1 === (Directions.N, Directions.S))
	  
	  val result2 = Directions.normalizeClockwise(Directions.NW, Directions.SE)
	  assert(result2 === (Directions.SE, Directions.NW))
	}
}