package mr.merc.map.hex

class TerrainHexFieldView(field:TerrainHexField) {
	val hexes = field.hexes.map(th => new TerrainHexView(th, field))
	
	private[hex] def hex (x:Int, y:Int) = hexes.find(h => h.hex.x == x && h.hex.y == y).get
}