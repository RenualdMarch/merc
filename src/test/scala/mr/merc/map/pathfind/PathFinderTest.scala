package mr.merc.map.pathfind

import mr.merc.map.UniversalGrid
import org.scalatest.FunSuite
import mr.merc.map.hex.Hex
import mr.merc.map.hex.HexField
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import mr.merc.map.objects._

class PathFinderTest extends FunSuite {
  val finder = PathFinder

  test("pathfinding sanity check") {
    val grid = new TestGrid(new HexField[Hex](5, 5, Hex.hexInit))
    val from = grid.hex(0, 1)
    val to = grid.hex(0, 3)
    val connector = grid.hex(0, 2)
    val result = finder.findPath(grid, from, to)
    assert(result.get === List(from, connector, to))
  }

  test("pathfinding with blocked path") {
    val grid = new TestGrid(new HexField[Hex](5, 5, Hex.hexInit)) {
      override def isBlocked(hex: Hex) = hex.x == 0 && hex.y == 2
    }

    val from = grid.hex(0, 1)
    val to = grid.hex(0, 3)
    val connector1 = grid.hex(1, 1)
    val connector2 = grid.hex(1, 2)

    val result = finder.findPath(grid, from, to)
    assert(result.get === List(from, connector1, connector2, to))
  }

  test("pathfinding with no path") {
    val grid = new TestGrid(new HexField[Hex](5, 5, Hex.hexInit)) {
      override def isBlocked(hex: Hex) = hex.x == 1
    }

    val from = grid.hex(0, 1)
    val to = grid.hex(4, 3)

    val result = finder.findPath(grid, from, to)
    assert(result.isEmpty)
  }

  test("pathfinding with cells where you can only to stop") {
    val grid = new TestGrid(new HexField[Hex](5, 5, Hex.hexInit)) {
      override def cellWhereMovementMustBeStopped(h: Hex) = {
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
    val grid = new TestGrid(new HexField[Hex](5, 5, Hex.hexInit)) {
      override def cellWhereItIsForbiddenToStop(h: Hex) = {
        h.x == 1 && h.y == 0
      }
    }

    val from = grid.hex(0, 0)
    val dest = grid.hex(1, 0)
    val result = finder.findPath(grid, from, dest)
    assert(result === None)
  }

  test("simple pathfinding") {
    val grid = new TestGrid(new TerrainHexField(10, 10, (x, y) => new TerrainHex(x, y, Grass)))
    val from = grid.hex(0, 0)
    val dest = grid.hex(5, 2)
    val result = finder.findPath(grid, from, dest)
    import grid.hex
    assert(result.get === List(hex(0, 0), hex(1, 0), hex(2, 1), hex(3, 1), hex(4, 2), hex(5, 2)))
  }

  test("complex pathfinding") {
    def mapInit(x: Int, y: Int) = if (x == 1 || x == 2) {
      new TerrainHex(x, y, Water, if (y == 2) Some(WoodenBridge) else None)
    } else if (y == 2) {
      new TerrainHex(x, y, Forest)
    } else if (x == 4 && y == 3) {
      new TerrainHex(x, y, Grass, Some(House))
    } else {
      new TerrainHex(x, y, Sand)
    }

    val costMap: Map[TerrainType, Int] = Map(Water -> 3, Forest -> 3, Swamp -> 4,
      Hill -> 2, Mountain -> 1000, Road -> 1, Sand -> 2, Grass -> 1)

    val field = new TestGrid(new TerrainHexField(5, 5, mapInit)) {
      override def price(from: TerrainHex, hex: TerrainHex) = costMap(hex.terrain)
    }
    val start = field.hex(4, 3)
    val finish = field.hex(1, 2)

    val path = finder.findPath(field, start, finish)
    val pathSum = path.get.tail.map(h => costMap(h.terrain)).sum
    assert(path.get === List(field.hex(4, 3), field.hex(3, 3), field.hex(2, 3), field.hex(1, 2)))
  }
}
