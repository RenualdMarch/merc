package mr.merc.map.view

import scalafx.geometry.Rectangle2D
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

class RectIntersectionHelper[T] {
  private val rects = new HashMap[T, Rectangle2D]()
  private val intersectionMap = HashMap[T, HashSet[T]]()

  def intersections(t: T) = intersectionMap(t).toSet

  def addRect(tag: T, rect: Rectangle2D) {
    rects += tag -> rect
    intersectionMap.remove(tag)
    removeFromIntersections(tag)
    calculateIntersections(tag)
  }

  def removeRect(tag: T) {
    rects -= tag
    intersectionMap.remove(tag)
    removeFromIntersections(tag)
  }

  private def removeFromIntersections(t: T) {
    intersectionMap.values.foreach(_ -= t)
  }

  private def calculateIntersections(t: T) {
    val currentRect = rects(t)
    val intersections = new HashSet[T]()
    rects.foreach {
      case (tag, rect) =>
        if (t != tag && rect.intersects(currentRect)) {
          intersections += tag
        }
    }
    intersectionMap += t -> intersections
    intersections foreach { inter =>
      intersectionMap(inter) += t
    }
  }
}