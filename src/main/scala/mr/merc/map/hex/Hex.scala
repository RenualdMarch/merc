package mr.merc.map.hex

object Hex {
  def hexInit(x:Int, y:Int) = new Hex(x, y)
  
  def apply(x:Int, y:Int) = new Hex(x, y)
}

class Hex(val x:Int, val y:Int) {

  override def toString() = s"Hex($x,$y)"
  
  private[hex] def toCubeHex:CubeHex = {
    val cubeX = x
    val cubeZ = y - (x - x&1) / 2
    val cubeY = -cubeX-cubeZ
    new CubeHex(cubeX, cubeY, cubeZ)
  }
  
  override def equals(any:Any) = any match {
    case hex:Hex => hex.x == x && hex.y == y
    case _ => false
  }
  
  override def hashCode = x + y
}

