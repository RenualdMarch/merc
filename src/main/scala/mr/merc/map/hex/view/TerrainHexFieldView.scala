package mr.merc.map.hex.view

import scalafx.scene.canvas.GraphicsContext
import mr.merc.map.hex.TerrainHexField


class TerrainHexFieldView(field:TerrainHexField) {
	val hexes = field.hexes.map(th => new TerrainHexView(th, field))
	
	private val map = hexes map (h => ((h.hex.x, h.hex.y), h)) toMap
	
	def hex (x:Int, y:Int) = map(x, y)
	
	def drawItself(gc:GraphicsContext) {
	  hexes.foreach(_.drawItself(gc))
	}
}