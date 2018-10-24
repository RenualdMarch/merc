package mr.merc.map.hex

import mr.merc.map.hex.view.DirectionsRange
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
    assert(DirectionsRange(NE, SW).contains(DirectionsRange(NE, S)))
    assert(!DirectionsRange(NE, S).contains(DirectionsRange(NE, SW)))
    assert(!DirectionsRange(NE, SW).contains(DirectionsRange(S, NW)))
    assert(DirectionsRange(NE, SW).contains(DirectionsRange(NE, SW)))
    assert(DirectionsRange(S, N).contains(DirectionsRange(SW, N)))
    assert(DirectionsRange(S, NE).contains(DirectionsRange(SW, N)))
    assert(DirectionsRange(NW, N).contains(DirectionsRange(NW, NW)))
    assert(DirectionsRange(NW, N).contains(DirectionsRange(N, N)))

  }

  test("overlapping slices") {
    assert(DirectionsRange(NE, SW).intersects(DirectionsRange(NE, S)))
    assert(!DirectionsRange(SW, NW).intersects(DirectionsRange(NE, SE)))
    assert(DirectionsRange(NW, SW).intersects(DirectionsRange(NE, SE)))
    assert(DirectionsRange(NE, SW).intersects(DirectionsRange(NE, SW)))
    assert(DirectionsRange(S, NE).intersects(DirectionsRange(NW, S)))
    assert(DirectionsRange(NW, N).intersects(DirectionsRange(NW, NW)))
    assert(DirectionsRange(NW, N).intersects(DirectionsRange(N, N)))
  }
}