package mr.merc.map.pathfind

import mr.merc.map.UniversalGrid
import mr.merc.map.hex.{Hex, HexField}


class TestGrid[T <: Hex](field: HexField[T]) extends UniversalGrid[T] {
  override def heuristic(from: T, to: T): Double = {
    val a = from.toCubeHex
    val b = to.toCubeHex

    import scala.math._
    max(abs(a.x - b.x), max(abs(a.y - b.y), abs(a.z - b.z)))
  }

  override def cellWhereMovementMustBeStopped(t: T): Boolean = false

  override def cellWhereItIsForbiddenToStop(t: T): Boolean = false

  override def isBlocked(t: T): Boolean = false

  override def price(from: T, to: T): Double = 1

  override def neighbours(t: T): List[T] = field.neighbours(t)

  def hex(x: Int, y: Int):T = field.hex(x, y)
}

