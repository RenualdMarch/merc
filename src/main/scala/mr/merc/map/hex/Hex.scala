package mr.merc.map.hex

class Hex(val x:Int, val y:Int) {

  override def toString() = s"Hex($x,$y)"
  
  private[hex] def toCubeHex:CubeHex = {
    val cubeX = x - (y - y&1) / 2
    val cubeZ = y
    val cubeY = -cubeX-cubeZ
    new CubeHex(cubeX, cubeY, cubeZ)
  }
}

