package mr.merc.map.hex

import scalafx.scene.canvas.GraphicsContext
import javafx.embed.swing.SwingFXUtils
import scalafx.scene.image.WritableImage
import mr.merc.image.MImage
import mr.merc.map.terrain.Grass
import mr.merc.map.terrain.Forest

class TerrainHexView(val hex:TerrainHex, val neighbours:Map[Directions.Value, TerrainHex]) {
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
	
	def image:MImage = {
	  if (hex.terrain == Forest) {
	    MImage(Grass.imagePath)
	  } else {
	    MImage(hex.terrain.imagePath)
	  }	  
	}
	  
	def secondaryImage:Option[MImage] = {
	  if (hex.terrain == Forest) {
	    Some(MImage(Forest.imagePath))
	  } else {
	    None
	  }
	}
	
	def drawItself(gc:GraphicsContext) {
	  image.drawCenteredImage(gc, x, y, side, side)
	  secondaryImage.map(_.drawCenteredImage(gc, x, y, side, side))
	  elements foreach (_.drawItself(gc, x, y))
	}
}