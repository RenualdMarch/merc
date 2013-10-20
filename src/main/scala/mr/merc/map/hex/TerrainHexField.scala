package mr.merc.map.hex

class TerrainHexField(width:Int, height:Int, init:(Int, Int) => TerrainHex) extends HexField[TerrainHex](width, height, init) {
	
}