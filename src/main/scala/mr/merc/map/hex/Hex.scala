package mr.merc.map.hex

class Hex(val x:Int, val y:Int) {

  override def toString() = s"Hex($x,$y)"
  
  private[hex] def toCubeHex:CubeHex = {
    val cubeX = x
    val cubeZ = y - (x - x&1) / 2
    val cubeY = -cubeX-cubeZ
    new CubeHex(cubeX, cubeY, cubeZ)
  }
}

