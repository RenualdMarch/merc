package mr.merc.map.hex

object Hex {
  def hexInit(x: Int, y: Int) = new Hex(x, y)

  def apply(x: Int, y: Int) = new Hex(x, y)
}

class Hex(val x: Int, val y: Int) {

  override def toString() = s"Hex($x,$y)"

  private[hex] def toCubeHex: CubeHex = {
    val cubeX = x
    val cubeZ = y - (x - (x & 1)) / 2
    val cubeY = -cubeX - cubeZ
    CubeHex(cubeX, cubeY, cubeZ)
  }

  private[hex] def toAxialHex = toCubeHex.toAxialHex

  override def equals(any: Any) = any match {
    case hex: Hex => hex.x == x && hex.y == y
    case _ => false
  }

  def distance(to: Hex): Int = {
    val fromCube = this.toCubeHex
    val toCube = to.toCubeHex
    (Math.abs(fromCube.x - toCube.x) + Math.abs(fromCube.y - toCube.y) + Math.abs(fromCube.z - toCube.z)) / 2
  }

  override def hashCode = x + y
}

