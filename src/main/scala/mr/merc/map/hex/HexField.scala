package mr.merc.map.hex

import scala.math._
import mr.merc.map.Grid
import scala.reflect.ClassTag

class HexField[T <: Hex: ClassTag](val width: Int, val height: Int, init: (Int, Int) => T) extends AbstractHexField[T](init) {
  private val arr = Array.ofDim[T](width, height)
  for (x <- 0 until width; y <- 0 until height) {
    if (isLegalCoords(x, y)) {
      arr(x)(y) = init(x, y)
    }
  }

  def isLegalCoords(x: Int, y: Int): Boolean = {
    if (x % 2 != 0 && y == height - 1) {
      return false
    }

    if (x >= width || y >= height || x < 0 || y < 0) {
      false
    } else {
      true
    }
  }

  def hex(x: Int, y: Int) = {
    require(isLegalCoords(x, y), s"x=$x and y=$y are illegal coords!")
    arr(x)(y)
  }

  def hexes: Seq[T] = {
    val indices = for (y <- 0 until height; x <- 0 until width) yield {
      (x, y)
    }

    indices.filter(i => isLegalCoords(i._1, i._2)).map(i => hex(i._1, i._2))
  }

  def map[K <: Hex: ClassTag](f:T => K):HexField[K] = {
    def buildHex(x: Int, y: Int):K = {
      f(this.hex(x, y))
    }

    new HexField[K](width, height, buildHex)
  }
}