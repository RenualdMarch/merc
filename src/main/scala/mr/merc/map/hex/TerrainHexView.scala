package mr.merc.map.hex

import scalafx.scene.canvas.GraphicsContext
import javafx.embed.swing.SwingFXUtils
import scalafx.scene.image.WritableImage
import mr.merc.image.MImage
import mr.merc.map.terrain.Grass
import mr.merc.map.terrain.Forest

class TerrainHexView(val hex:TerrainHex, field:TerrainHexField) {
    val neighbours = field.neighboursWithDirections(hex.x, hex.y)
	val side = 72
	val x = findX
	val y = findY
	
	private def findX = if (hex.x % 2 == 0) {
	  hex.x * 6 * side / 8
	} else {
	  (hex.x - 1) * 6 * side / 8 + side * 3 / 4 
	}
	
	private def findY = if (hex.x % 2 == 0) {
	  side * hex.y
	} else {
	  side * hex.y + side / 2
	}
	
	private lazy val elements:List[TerrainHexViewAdditiveElement] = {
	  val additives = TerrainHexViewAdditive.extractAdditives(this)
	  val rule = new TerrainHexViewAdditiveRule
	  rule.transform(additives)
	}
	
	val image:MImage = {
	  if (hex.terrain == Forest) {
	    MImage(Grass.imagePath)
	  } else {
	    MImage(hex.terrain.imagePath)
	  }	  
	}
	  
	val secondaryImage:Option[MImage] = {
	  if (hex.terrain == Forest) {
	    Some(MImage(Forest.imagePath))
	  } else {
	    None
	  }
	}
	
	val mapObjects:List[MImage] = {
	  // map object of this field must be drawn last to be higher
	  var objects = List[MImage]()
	  hex.mapObj match {
	    case Some(mapObj) => objects :::= mapObj.images(hex, field)
	    case None =>
	  }
	  
	  // then go map objects for neighbors
	  val neigMapObj = field.neighbours(hex).filter(p => p.mapObj != None && p.mapObj != hex.mapObj)
	  val neigImages = neigMapObj.flatMap(p => p.mapObj.get.images(hex, field)).toList
	  objects :::= neigImages
	  objects
	}
	
	def drawItself(gc:GraphicsContext) {
	  image.drawCenteredImage(gc, x, y, side, side)
	  elements foreach (_.drawItself(gc, x, y))
	  mapObjects foreach (_.drawCenteredImage(gc, x, y, side, side))
	  secondaryImage.map(_.drawCenteredImage(gc, x, y, side, side))
	}
}