package mr.merc.map.hex

private[hex] case class CubeHex(x: Int, y: Int, z: Int) {

  def toHex: Hex = {
    val q = x
    val r = z + (x - (x & 1)) / 2
    new Hex(q, r)
  }

  def toAxialHex = new AxialHex(x, z)
}