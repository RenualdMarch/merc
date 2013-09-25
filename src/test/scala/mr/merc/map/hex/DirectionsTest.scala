package mr.merc.map.hex

import org.scalatest.FunSuite

class DirectionsTest extends FunSuite {
	test("in the middle") {
	  val set = S.neighbours
	  assert(set === Set(SE, SW))
	}
	
	test("beginning") {
	  val set = N.neighbours
	  assert(set === Set(NE, NW))
	}
	
	test("end") {
	  val set = NW.neighbours
	  assert(set === Set(SW, N))
	}
		
	test("comparing slices") {
	  assert(Direction.leftSliceContainsRightSlice((NE, SW), (NE, S)))
	  assert(!Direction.leftSliceContainsRightSlice((NE, S), (NE, SW)))
	  assert(!Direction.leftSliceContainsRightSlice((NE, SW), (S, NW)))
	  assert(Direction.leftSliceContainsRightSlice((NE, SW), (NE, SW)))
	  assert(Direction.leftSliceContainsRightSlice((S, N), (SW, N)))
	  assert(Direction.leftSliceContainsRightSlice((S, NE), (SW, N)))
	  
	  assert(Direction.leftSliceContainsRightSlice((NW, N), (NW, NW)))
	  assert(Direction.leftSliceContainsRightSlice((NW, N), (N, N)))
	  
	}
	
	test("overlapping slices") {
	  assert(Direction.overlapping((NE, SW), (NE, S)))
	  assert(!Direction.overlapping((SW, NW), (NE, SE)))
	  assert(Direction.overlapping((NW, SW), (NE, SE)))
	  assert(Direction.overlapping((NE, SW), (NE, SW)))
  	  assert(Direction.overlapping((S, NE), (NW, S)))
  	  assert(Direction.overlapping((NW, N), (NW, NW)))
	  assert(Direction.overlapping((NW, N), (N, N)))

	  
	}
}