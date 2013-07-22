package mr.merc.map.hex

import scalafx.scene.canvas.GraphicsContext
import javafx.embed.swing.SwingFXUtils
import scalafx.scene.image.WritableImage
import mr.merc.image.MImage

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
	
	def image = MImage(hex.terrain.imagePath)
	
	def drawItself(gc:GraphicsContext) {
	  gc.drawImage(image.image, x, y)
	}
}