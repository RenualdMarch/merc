package mr.merc.map.objects

import mr.merc.map.hex._
import org.scalatest.FunSuite

class WallTest extends FunSuite {

  private val fullSet = Set(N, NE, NW, S, SE, SW)

  test("add not connected wall") {
    val result = Walls().addWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = false).
      addWall(Wall(new Hex(3, 3), fullSet), removeInnerWall = false)

    assert(result.keepWalls === Nil)
    assert(result.walls.toSet === Set(Wall(new Hex(1, 1), fullSet),
      Wall(new Hex(3, 3), fullSet)))
  }

  test("add connected wall without removing inner wall") {
    val result = Walls().addWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = false).
      addWall(Wall(new Hex(2, 2), fullSet), removeInnerWall = false)

    assert(result.keepWalls === Nil)
    assert(result.walls.toSet === Set(Wall(new Hex(1, 1), fullSet),
      Wall(new Hex(2, 2), Set(N, NE, S, SE, SW))))
  }

  test("add connected wall with removing of inner wall") {
    val result = Walls().addWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = false).
      addWall(Wall(new Hex(2, 2), fullSet), removeInnerWall = true)

    assert(result.keepWalls === Nil)
    assert(result.walls.toSet === Set(Wall(new Hex(1, 1), Set(N, NE, NW, S, SW)),
      Wall(new Hex(2, 2), Set(N, NE, S, SE, SW))))
  }

  test("add one keep wall with inner walls") {
    val result1 = Walls().addWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = false).
      addWall(Wall(new Hex(2, 2), fullSet), removeInnerWall = false).
      addKeepWall(Wall(new Hex(2, 1), fullSet), removeInnerWall = false)

    val result2 = Walls().addKeepWall(Wall(new Hex(2, 1), fullSet), removeInnerWall = false).
      addWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = false).
      addWall(Wall(new Hex(2, 2), fullSet), removeInnerWall = false)

    assert(result1 === result2)
    assert(result1 === Walls(List(Wall(new Hex(1, 1), Set(N, S, SE, SW, NW)), Wall(new Hex(2, 2), Set(NE, S, SE, SW))), List(Wall(new Hex(2, 1), fullSet))))
  }

  test("add one keep wall without inner walls") {
    val result1 = Walls().addWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = true).
      addWall(Wall(new Hex(2, 2), fullSet), removeInnerWall = true).
      addKeepWall(Wall(new Hex(2, 1), fullSet), removeInnerWall = true)

    val result2 = Walls().addKeepWall(Wall(new Hex(2, 1), fullSet), removeInnerWall = true).
      addWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = true).
      addWall(Wall(new Hex(2, 2), fullSet), removeInnerWall = true)

    assert(result1 === result2)
    assert(result1 === Walls(List(Wall(new Hex(1, 1), Set(N, NW, S, SW)), Wall(new Hex(2, 2), Set(NE, S, SE, SW))), List(Wall(new Hex(2, 1), fullSet))))
  }

  test("add keeper walls with intersections, removing inner walls") {
    val result1 = Walls().
      addWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = true).
      addWall(Wall(new Hex(2, 2), fullSet), removeInnerWall = true).
      addKeepWall(Wall(new Hex(2, 1), fullSet), removeInnerWall = true).
      addKeepWall(Wall(new Hex(3, 1), fullSet), removeInnerWall = true)

    val result2 = Walls().
      addKeepWall(Wall(new Hex(2, 1), fullSet), removeInnerWall = true).
      addKeepWall(Wall(new Hex(3, 1), fullSet), removeInnerWall = true).
      addWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = true).
      addWall(Wall(new Hex(2, 2), fullSet), removeInnerWall = true)

    assert(result1 === result2)
    assert(result1 === result2)
    assert(result1 === Walls(List(Wall(new Hex(1, 1), Set(N, NW, S, SW)), Wall(new Hex(2, 2), Set(S, SE, SW))),
      List(Wall(new Hex(2, 1), Set(N, NE, NW, S, SW)), Wall(new Hex(3, 1), Set(N, NE, S, SE, SW)))))
  }

  test("add keeper walls with intersections, retaining inner walls") {
    val result1 = Walls().
      addWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = false).
      addWall(Wall(new Hex(2, 2), fullSet), removeInnerWall = false).
      addKeepWall(Wall(new Hex(2, 1), fullSet), removeInnerWall = false).
      addKeepWall(Wall(new Hex(3, 1), fullSet), removeInnerWall = false)

    val result2 = Walls().
      addKeepWall(Wall(new Hex(2, 1), fullSet), removeInnerWall = false).
      addKeepWall(Wall(new Hex(3, 1), fullSet), removeInnerWall = false).
      addWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = false).
      addWall(Wall(new Hex(2, 2), fullSet), removeInnerWall = false)

    assert(result1 === result2)
    assert(result1 === result2)
    assert(result1 === Walls(List(Wall(new Hex(1, 1), Set(N, NW, S, SW, SE)), Wall(new Hex(2, 2), Set(S, SE, SW))),
      List(Wall(new Hex(2, 1), fullSet), Wall(new Hex(3, 1), Set(N, NE, S, SE, SW)))))
  }

  test("cann't add already present wall") {
    intercept[RuntimeException] {
      Walls().addWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = false).
        addWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = false)
    }

    intercept[RuntimeException] {
      Walls().addWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = false).
        addKeepWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = false)
    }

    intercept[RuntimeException] {
      Walls().addKeepWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = false).
        addKeepWall(Wall(new Hex(1, 1), fullSet), removeInnerWall = false)
    }

  }
}
