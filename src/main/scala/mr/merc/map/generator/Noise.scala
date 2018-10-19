package mr.merc.map.generator

import scala.util.Random

trait Noise {

  def apply(x: Double, y:Double): Double

  def apply(x: Double, width: Double, y: Double, height: Double): Double = {
    apply(x/width, y/height)
  }

  def add(q: Double, noise: Noise): Noise = {
    new NoiseSum(Map(1d -> this, q -> noise))
  }

  private val samplesSize = 10000
  private lazy val noiseSamples = (for {x <- 0 until 100; y <- 0 until 100} yield {this.apply(x / 100d, y / 100d)}).toVector.sorted

  // 0 is 0%, 1 is 100%
  def percentageBelow(percentage:Double):Double = {
    val index = (percentage * (samplesSize - 1)).toInt
    noiseSamples(index)
  }

  def applyFunction(f: ((Double, Double), Double) => Double): Noise = {
      (x, y) => f((x, y), Noise.this.apply(x, y))
  }
}

object Noise {
  def apply(seed:Int, frequency: Double): Noise = new NoiseProducer(seed, frequency)

  def apply(frequency: Double): Noise = new NoiseProducer(frequency = frequency)
}

class NoiseProducer(seed:Int = Random.nextInt(), frequency: Double = 1) extends Noise {
  private val algorithm = new NoiseAlgorithm(seed)

  override def apply(x: Double, y:Double): Double = {
    algorithm.noise(frequency * x, frequency * y, 0)
  }
}

class NoiseSum(noises: Map[Double, Noise]) extends Noise {
  override def apply(x: Double, y: Double): Double = {
    val v = noises.map{case (q, n) => q * n.apply(x, y)}.sum
    val s = noises.keys.sum
    v / s
  }

  override def add(q: Double, noise: Noise): Noise = {
    new NoiseSum(noises + (q -> noise))
  }
}
