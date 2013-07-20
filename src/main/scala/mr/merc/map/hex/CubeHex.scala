package mr.merc.map.hex

private[hex] class CubeHex(val x:Int, val y:Int, val z:Int) {

  def toHex:Hex = {
    val q = x
    val r = z + (x - x&1) / 2
    new Hex(q, r)
  }
}