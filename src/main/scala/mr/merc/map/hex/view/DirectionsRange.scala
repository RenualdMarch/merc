package mr.merc.map.hex.view

import mr.merc.map.hex._

import scala.collection.BitSet


object DirectionsRange {
  def empty: DirectionsRange = DirectionsRange(BitSet.empty)

  def apply(direction: Direction): DirectionsRange = {
    DirectionsRange(BitSet(number(direction)))
  }

  def apply(from: Direction, to: Direction): DirectionsRange = {
    if (from == to) {
      apply(from)
    } else {
      val fromInt = number(from)
      val toInt = number(to)
      if (fromInt < toInt) {
        DirectionsRange(BitSet() ++ number(from).to(number(to)))
      } else {
        val newToInt = toInt + 6
        DirectionsRange(BitSet() ++ fromInt.to(newToInt).map(_ % 6))
      }
    }
  }

  def number(d: Direction): Int = d match {
    case N => 0
    case NE => 1
    case SE => 2
    case S => 3
    case SW => 4
    case NW => 5
  }

  def direction(n: Int): Direction = n match {
    case 0 => N
    case 1 => NE
    case 2 => SE
    case 3 => S
    case 4 => SW
    case 5 => NW
    case x => direction(x % 6)
  }
}

case class DirectionsRange(directions: BitSet) {
  def intersects(dr: DirectionsRange): Boolean = {
    this.directions.intersect(dr.directions).nonEmpty
  }

  def isEmpty: Boolean = directions.isEmpty

  def isFull: Boolean = directions.size == 6

  def size: Int = directions.size

  def contains(dr: DirectionsRange): Boolean = {
    dr.directions.subsetOf(this.directions)
  }

  def +(dr: DirectionsRange): DirectionsRange = {
    DirectionsRange(this.directions ++ dr.directions)
  }

  def canBeUnited(dr: DirectionsRange): Boolean = {
    (this + dr).toDirPair.isDefined
  }

  def toDirPair: Option[(Direction, Direction)] = {
    if (isFull) {
      return Some(N, NW)
    }

    val sorted = directions.toList.sorted

    import DirectionsRange._
    val pair = sorted.headOption.map { start =>
      val from = direction(start)
      var to = from
      while (directions.contains(number(to))) {
        to = to.next
      }

      to = to.prev
      (from, to)
    }

    pair.flatMap { case (from, to) =>
      if (apply(from, to) == this) {
        Some(from, to)
      } else {
        None
      }
    }
  }

  override def toString: String = {
    s"DirectionsRange(${directions.map(DirectionsRange.direction)})"
  }
}
