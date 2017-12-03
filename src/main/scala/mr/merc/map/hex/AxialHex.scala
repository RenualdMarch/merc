package mr.merc.map.hex

private[hex] case class AxialHex(x:Int, y:Int) {
	def toCube = CubeHex(x, -x-y , y)
	def toHex = toCube.toHex
}