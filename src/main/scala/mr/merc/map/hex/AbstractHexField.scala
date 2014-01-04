package mr.merc.map.hex

import mr.merc.map.Grid
import scala.reflect.ClassTag

abstract class AbstractHexField[T <: Hex](init: (Int, Int) => T) extends Grid[T] {

  def isLegalCoords(x: Int, y: Int): Boolean

  def hex(x: Int, y: Int): T

  def neighbours(hex: T): Set[T] = neighbours(hex.x, hex.y)

  def neighbours(x: Int, y: Int): Set[T] = neighboursList(x, y).toSet

  private def neighboursList(x: Int, y: Int): List[T] = {
    val allNeighboursCoords = neighboursListWithInvalid(x, y)
    val correctCoords = allNeighboursCoords.filter(h => isLegalCoords(h._1, h._2))
    correctCoords.map(h => hex(h._1, h._2))
  }

  private def neighboursListWithInvalid(x: Int, y: Int): List[(Int, Int)] = {
    // x % 2 == 1 is even because we start from zero
    val corrections = correctionsList(x % 2 == 1)
    corrections.map(h => (h._1 + x, h._2 + y))
  }

  def neighboursWithDirections(hex: T): Map[Direction, T] = neighboursWithDirections(hex.x, hex.y)

  def neighboursWithDirections(x: Int, y: Int): Map[Direction, T] = {
    val resultList = (directionsList zip neighboursListWithInvalid(x, y)).filter(dh => isLegalCoords(dh._2._1, dh._2._2))
    resultList.map(df => (df._1, hex(df._2._1, df._2._2))).toMap
  }

  def direction(hex: T, neig: T) = neighboursWithDirections(hex).find(_._2 == neig).get._1

  private def correctionsList(even: Boolean): List[(Int, Int)] = {
    even match {
      case true => List((-1, 0), (0, -1), (1, 0), (1, 1), (0, 1), (-1, 1))
      case false => List((-1, -1), (0, -1), (1, -1), (1, 0), (0, 1), (-1, 0))
    }
  }

  private val directionsList = List(NW, N, NE, SE, S, SW)
  def distance(from: T, to: T) = from.distance(to)
}