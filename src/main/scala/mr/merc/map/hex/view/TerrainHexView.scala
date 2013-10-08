package mr.merc.map.hex.view

import scalafx.scene.canvas.GraphicsContext
import mr.merc.image.MImage
import mr.merc.map.terrain.Grass
import mr.merc.map.terrain.Forest
import mr.merc.map.hex.TerrainHex
import mr.merc.map.hex.TerrainHexField

class TerrainHexView(val hex:TerrainHex, field:TerrainHexField) {
    val neighbours = field.neighboursWithDirections(hex.x, hex.y)
    val directions = neighbours map (p => (p._2, p._1)) toMap
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
	
	val mapObject = hex.mapObj match {
	    case Some(mapObj) => mapObj.images(hex, field)
	    case None => Nil
	  }	  
	
	
	val neighbourMapObjects:List[MImage] = {
	  val neigMapObj = field.neighbours(hex).filter(p => p.mapObj != None && p.mapObj != hex.mapObj)
	  neigMapObj.flatMap(p => p.mapObj.get.images(hex, field)).toList
	}
	
	def drawItself(gc:GraphicsContext) {
	  image.drawCenteredImage(gc, x, y, side, side)
	  elements foreach (_.drawItself(gc, x, y))
	  neighbourMapObjects foreach (_.drawCenteredImage(gc, x, y, side, side))
	  secondaryImage.map(_.drawCenteredImage(gc, x, y, side, side))
	  mapObject foreach (_.drawCenteredImage(gc, x, y, side, side))	 
	}
}