package mr.merc.map.hex

private[hex] case class CubeHex(x: Int, y: Int, z: Int) {

  def toHex: Hex = {
    val q = x
    val r = z + (x - (x & 1)) / 2
    new Hex(q, r)
  }

  def toAxialHex = AxialHex(x, z)

  private val cubeDirections = Map(SE -> (+1, -1, 0), NE -> (+1, 0, -1), N -> (0, +1, -1),
    NW -> (-1, +1, 0), SW -> (-1, 0, +1), S -> (0, -1, +1))

  def neighbour(direction: Direction, step:Int = 1):CubeHex = {
    val (xx, yy, zz) = cubeDirections(direction)
    CubeHex(x + step * xx, y + step * yy, z + step * zz)
  }
}