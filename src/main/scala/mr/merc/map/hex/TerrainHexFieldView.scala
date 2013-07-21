package mr.merc.map.hex

class TerrainHexFieldView(field:TerrainHexField) {
	val hexes = field.hexes.map(new TerrainHexView(_))
}