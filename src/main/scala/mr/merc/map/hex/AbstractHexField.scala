package mr.merc.map.hex

import mr.merc.map.{Grid, ShortestGrid, UniversalGrid}
import mr.merc.map.pathfind.PathFinder
import mr.merc.util.MercUtils

abstract class AbstractHexField[T <: Hex](init: (Int, Int) => T) {

  def isLegalCoords(x: Int, y: Int): Boolean

  def hex(x: Int, y: Int): T

  def hexOpt(x:Int, y:Int): Option[T] = {
    if (isLegalCoords(x, y)) Some(hex(x, y)) else None
  }

  def neighbours(hex: T): Set[T] = neighbours(hex.x, hex.y)

  def neighbours(x: Int, y: Int): Set[T] = neighboursList(x, y).toSet

  private def neighboursList(x: Int, y: Int): List[T] = {
    val allNeighboursCoords = neighboursListWithInvalid(x, y)
    val correctCoords = allNeighboursCoords.filter(h => isLegalCoords(h._1, h._2))
    correctCoords.map(h => hex(h._1, h._2))
  }

  private def neighboursListWithInvalid(x: Int, y: Int): List[(Int, Int)] = {
    // x % 2 == 1 is even because we start from zero
    val corrections = correctionsList(x % 2 != 0)
    corrections.map(h => (h._1 + x, h._2 + y))
  }

  def neighboursWithDirections(hex: T): Map[Direction, T] = neighboursWithDirections(hex.x, hex.y)

  def neighboursWithDirections(x: Int, y: Int): Map[Direction, T] = {
    val resultList = (directionsList zip neighboursListWithInvalid(x, y)).filter(dh => isLegalCoords(dh._2._1, dh._2._2))
    resultList.map(df => (df._1, hex(df._2._1, df._2._2))).toMap
  }

  def direction(hex: T, neig: T):Direction = neighboursWithDirections(hex).find(_._2 == neig).get._1

  private def correctionsList(even: Boolean): List[(Int, Int)] = {
    even match {
      case true => List((-1, 0), (0, -1), (1, 0), (1, 1), (0, 1), (-1, 1))
      case false => List((-1, -1), (0, -1), (1, -1), (1, 0), (0, 1), (-1, 0))
    }
  }

  private val directionsList = List(NW, N, NE, SE, S, SW)

  def distance(from: T, to: T):Int = from.distance(to)

  def hexRing(hex: T, radius:Int):List[T] = {
    if (radius == 0) List(hex)
    else {
      val cube = hex.toCubeHex.neighbour(SW, radius)

      val movements = for {
        dir <- List(SE, NE, N, NW, SW, S)
        _ <- 0 until radius
      } yield {
        dir
      }

      val cubes = movements.scanLeft(cube) {(c, direction) =>
        c.neighbour(direction)
      }

      cubes.tail.flatMap {c =>
        val h = c.toHex
        this.hexOpt(h.x, h.y)
      }
    }
  }

  def closest(hex:T):Stream[T] = {
    case class Ring(radius: Int) {
      private lazy val ring = hexRing(hex, radius)
      private def isEmpty = ring.isEmpty
      def ringHexes:Stream[T] = if (isEmpty) Stream.Empty else
        ring.toStream #::: Ring(radius + 1).ringHexes
    }

    Ring(0).ringHexes
  }

  def findClosest(start: T, predicate: T => Boolean):Option[T] = {
    closest(start).find(predicate)
  }

  def findPath(from: T, to: T, blocking:T => Boolean):Option[List[T]] = {
    val grid = new ShortestGrid[T] {
      override def isBlocked(t: T) = blocking(t)
      override def price(from: T, to: T): Double = 1
      override def neighbours(t: T): Set[T] = AbstractHexField.this.neighbours(t)
      override def heuristic(from: T, to: T): Double = math.abs(from.x - to.x) + math.abs(from.y - to.y)
    }

    PathFinder.findPath(grid, from, to)
  }
}