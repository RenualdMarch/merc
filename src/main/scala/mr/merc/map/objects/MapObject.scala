package mr.merc.map.objects

import mr.merc.image.MImage
import mr.merc.map.hex.TerrainHex
import mr.merc.map.hex.Directions
import mr.merc.map.hex.TerrainHexField


abstract class MapObject(val name:String) {
	def images(hex:TerrainHex, field:TerrainHexField):List[MImage]
	
	private[objects] def imagePath(fileName:String) = "/images/terrain/" + name + "/" + fileName + ".png"
}

