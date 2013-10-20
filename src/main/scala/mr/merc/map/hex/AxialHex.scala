package mr.merc.map.hex

private[hex] case class AxialHex(x:Int, y:Int) {
	def toCube = new CubeHex(x, -x-y , y)
	def toHex = toCube.toHex
}