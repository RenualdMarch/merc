package mr.merc.util


import scalafx.scene.paint.Color

import java.util
import scala.collection.generic.CanBuildFrom
import scala.util.Random

object MercUtils {
  implicit class RandomElement[T](trav: Traversable[T]) {
    def randomElement(): T = {
      if (trav.isEmpty) sys.error("traversable is empty!")
      else trav.toIndexedSeq(Random.nextInt(trav.size))
    }
  }

  implicit class Randomize(seq:Seq[Int]) {
    def shuffle(n: Int):Seq[Int] = {
      (0 until n).foldLeft(seq){case (s, _)=> makeOneShuffle(s)}
    }

    private def makeOneShuffle(s:Seq[Int]):Seq[Int] = {
      val from = Random.nextInt(s.size)
      val to = Random.nextInt(s.size)
      if (from != to && s(from) > 1) {
        s.updated(from, s(from) - 1).updated(to, s(to) + 1)
      } else s
    }
  }

  def printTime[T](message:String)(f: => T): T = {
    val before = System.currentTimeMillis()
    val res = f
    println(message + " " + (System.currentTimeMillis() - before).toString)
    res
  }

  def stablePseudoRandomIndex(x: Int, y: Int, size: Int): Int = {
    (x * 19 + y * 17) % size
  }

  def colorToStyle(color: Color):String = {
    s"rgb(${(color.red * 255).toInt},${(color.green * 255).toInt},${(color.blue * 255).toInt})"
  }

  def concurrentMutableSet[T]() = {
    import scala.collection.JavaConverters._
    java.util.Collections.newSetFromMap(
      new java.util.concurrent.ConcurrentHashMap[T, java.lang.Boolean]).asScala
  }

  implicit class MedianFinder[T](points:Traversable[(T, T)])(implicit numeric: Numeric[T], ordering: Ordering[T]) {
    def medianBySquares:(T, T) = {
      import numeric._
      points.map{ case (x, y) =>
          val sum = points.map { case (x1, y1) =>
            abs(x - x1) + abs(y - y1)
          }.sum
        ((x, y), sum)
      }.minBy(_._2)._1
    }
  }

  private val romanNumbersMap:util.TreeMap[Int, String] = {
    val map = new util.TreeMap[Int, String]()

    map.put(1000, "M")
    map.put(900, "CM")
    map.put(500, "D")
    map.put(400, "CD")
    map.put(100, "C")
    map.put(90, "XC")
    map.put(50, "L")
    map.put(40, "XL")
    map.put(10, "X")
    map.put(9, "IX")
    map.put(5, "V")
    map.put(4, "IV")
    map.put(1, "I")

    map
  }

  implicit class ArabicToRoman[T](x: T)(implicit n:Numeric[T]) {

    def toRomanString: String = {
      val number = n.toInt(x)
      val low = romanNumbersMap.floorKey(number)
      if ( number == low) {
        romanNumbersMap.get(number)
      } else {
        romanNumbersMap.get(low) + toRomanString(number - low)
      }
    }
  }

  implicit class AddToMapByKey[K, V](map:Map[K, List[V]]) {

    def addToList(k:K, v:V):Map[K, List[V]] = {
      val list = map.getOrElse(k, Nil)
      map + (k -> (v :: list))
    }

  }
}