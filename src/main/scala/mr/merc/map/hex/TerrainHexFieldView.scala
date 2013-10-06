package mr.merc.map.hex

class TerrainHexFieldView(field:TerrainHexField) {
	val hexes = field.hexes.map(th => new TerrainHexView(th, field))
	
	private val map = hexes map (h => ((h.hex.x, h.hex.y), h)) toMap
	
	def hex (x:Int, y:Int) = map(x, y)
}