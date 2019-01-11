package mr.merc.util

import scala.collection.mutable
import scala.util.Random

class WeightedRandom[T](weights:Map[T, Double]){

  private val weightsList = weights.toVector
  private val sum = weights.values.sum

  def nextRandomItem():T = {
    val rand = Random.nextDouble() * sum
    var s = rand
    for (i <- weightsList.indices) {
      s -= weightsList(i)._2
      if (s < 0) {
        return weightsList(i)._1
      }
    }

    weightsList.last._1
  }

  def uniqueRandomItems(n: Int): Set[T] = {
    require(n <= weights.size, s"n $n can't be bigger than ${weights.size}")

    val set = mutable.Set[T]()
    while (set.size < n) {
      set += nextRandomItem()
    }

    set.toSet
  }
}
