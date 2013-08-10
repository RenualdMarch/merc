package mr.merc.map.objects

import mr.merc.map.hex.TerrainHex
import mr.merc.image.MImage
import mr.merc.map.hex.TerrainHexField

object House extends MapObject("house") {
	def images(hex:TerrainHex, field:TerrainHexField):List[MImage] = {
	  if (hex.mapObj == Some(House)) {
	    List(MImage(imagePath("house")))
	  } else {
	    Nil
	  }
	}
	
}