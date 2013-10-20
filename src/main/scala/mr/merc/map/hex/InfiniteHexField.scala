package mr.merc.map.hex

class InfiniteHexField[T <: Hex](init:(Int, Int) => T) extends AbstractHexField[T](init) {
	def isLegalCoords(x:Int, y:Int) = true
  
	def hex(x:Int, y:Int) = init(x, y)
}