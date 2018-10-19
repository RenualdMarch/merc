package mr.merc.map.generator

import org.scalatest.{FunSuite, Matchers}

class NoiseGeneratorTest extends FunSuite with Matchers {
  private val noise = Noise(frequency = 77)

  test("avg noise") {
    val width = 1000
    val height = 1000

    val iterable = for {
      x <- 0 until width
      y <- 0 until height
    } yield {
      noise(x, width, y, height)
    }

    val sum = iterable.foldLeft(BigDecimal(0))(_ + _) / (width * height)
    sum.toDouble shouldBe (0d +- 0.01)

  }

  test("min/max noise") {
    val width = 1000
    val height = 1000

    var max = -1d
    var min = 1d

    for {
      x <- 0 until width
      y <- 0 until height
    } {
      val n = noise(x, width, y, height)
      max = Math.max(max, n)
      min = Math.min(min, n)
    }

    max should be < 1.1
    min should be > -1.1
  }

  test("sum of noises") {
    val noise = Noise(77).add(10, Noise(50)).add(0.5, Noise(1))
    val width = 1000
    val height = 1000

    var max = -1d
    var min = 1d

    for {
      x <- 0 until width
      y <- 0 until height
    } {
      val n = noise(x, width, y, height)
      max = Math.max(max, n)
      min = Math.min(min, n)
    }

    max should be < 1.1
    min should be > -1.1
  }

  test("apply function") {
    val noise = new Noise {
      override def apply(x: Double, y: Double): Double = 10
    }

    val fNoise = noise.applyFunction{case((x, y), n) =>
        x + y + n
    }

    assert(fNoise.apply(0.5, 0.6) === 11.1)
    assert(fNoise.apply(1, 5, 2, 5) === 10.6)
  }
}
