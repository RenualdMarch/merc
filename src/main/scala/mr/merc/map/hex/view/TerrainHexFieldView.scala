package mr.merc.map.hex.view

import scalafx.scene.canvas.GraphicsContext
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.CubeHex
import mr.merc.map.hex.Hex
import mr.merc.map.hex.InfiniteHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._

class TerrainHexFieldView(field:TerrainHexField) {
	val hexes = field.hexes.map(th => new TerrainHexView(th, field))
	
	private val map = hexes map (h => ((h.hex.x, h.hex.y), h)) toMap
	
	def hex (x:Int, y:Int) = map(x, y)
	
	def drawItself(gc:GraphicsContext) {
	  hexes.foreach(_.drawItself(gc))
	}
	
	
	private val infiniteField = new InfiniteHexField((x, y) => new TerrainHex(x, y, Grass))
	def hexByPixelCoords(pixelX:Int, pixelY:Int):Option[TerrainHexView] = {
	  val side = TerrainHexView.Side
	  // first part is transform hexes into squares 
	  // and decide to what square belongs point
	  // odd rows are +36 on y
	  // side is 3/4 * hex side
	  
	  val column = pixelX / side
	  val row = if (column % 2 == 0) {
	    pixelY / side
	  } else {
	    (pixelY - side / 2) / side
	  }
	  
	  val hex = infiniteField.hex(column, row)
	  val neighbours = infiniteField.neighbours(hex)
	  val candidates = neighbours + hex map (h => new TerrainHexView(h, field))
	  def distanceSquare(x:Int, y:Int) = (x - pixelX) * (x - pixelX) + (y - pixelY) * (y - pixelY)
	  val min = candidates.minBy(c => distanceSquare(c.center._1, c.center._2))
	
	  map.get(min.hex.x, min.hex.y)
	}
}