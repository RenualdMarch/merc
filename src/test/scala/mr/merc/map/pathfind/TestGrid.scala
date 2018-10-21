package mr.merc.map.pathfind

import mr.merc.map.UniversalGrid
import mr.merc.map.hex.{Hex, HexField}


class TestGrid[T <: Hex](field: HexField[T]) extends UniversalGrid[T] {
  override def heuristic(from: T, to: T): Double = math.abs(from.x - to.x) + math.abs(from.y - to.y)

  override def cellWhereMovementMustBeStopped(t: T): Boolean = false

  override def cellWhereItIsForbiddenToStop(t: T): Boolean = false

  override def isBlocked(t: T): Boolean = false

  override def price(from: T, to: T): Double = 1

  override def neighbours(t: T): List[T] = field.neighbours(t)

  def hex(x: Int, y: Int):T = field.hex(x, y)
}

