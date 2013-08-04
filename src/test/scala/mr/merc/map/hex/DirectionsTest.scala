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
		
	test("comparing slices") {
	  assert(Directions.leftSliceContainsRightSlice((Directions.NE, Directions.SW), (Directions.NE, Directions.S)))
	  assert(!Directions.leftSliceContainsRightSlice((Directions.NE, Directions.S), (Directions.NE, Directions.SW)))
	  assert(!Directions.leftSliceContainsRightSlice((Directions.NE, Directions.SW), (Directions.S, Directions.NW)))
	  assert(Directions.leftSliceContainsRightSlice((Directions.NE, Directions.SW), (Directions.NE, Directions.SW)))
	  assert(Directions.leftSliceContainsRightSlice((Directions.S, Directions.N), (Directions.SW, Directions.N)))
	  assert(Directions.leftSliceContainsRightSlice((Directions.S, Directions.NE), (Directions.SW, Directions.N)))
	  
	  assert(Directions.leftSliceContainsRightSlice((Directions.NW, Directions.N), (Directions.NW, Directions.NW)))
	  assert(Directions.leftSliceContainsRightSlice((Directions.NW, Directions.N), (Directions.N, Directions.N)))
	  
	}
	
	test("overlapping slices") {
	  assert(Directions.overlapping((Directions.NE, Directions.SW), (Directions.NE, Directions.S)))
	  assert(!Directions.overlapping((Directions.SW, Directions.NW), (Directions.NE, Directions.SE)))
	  assert(Directions.overlapping((Directions.NW, Directions.SW), (Directions.NE, Directions.SE)))
	  assert(Directions.overlapping((Directions.NE, Directions.SW), (Directions.NE, Directions.SW)))
  	  assert(Directions.overlapping((Directions.S, Directions.NE), (Directions.NW, Directions.S)))
  	  assert(Directions.overlapping((Directions.NW, Directions.N), (Directions.NW, Directions.NW)))
	  assert(Directions.overlapping((Directions.NW, Directions.N), (Directions.N, Directions.N)))

	  
	}
}