package mr.merc.util

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
}
