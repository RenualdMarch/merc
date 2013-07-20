package mr.merc.map.hex

private[hex] class CubeHex(val x:Int, val y:Int, val z:Int) {

  def toHex:Hex = {
    val q = x + (z - z&1) / 2
    val r = z
    new Hex(q, r)
  }
}