package mr.merc.unit.view

import scalafx.scene.paint.Color
import scalafx.scene.canvas.GraphicsContext

// fillPercentage is between 0 and 1
class VerticalBarView(width:Int, height:Int, borderColor:Color, fillColor:Color, private var _fillPercentage:Double) {
	def fillPercentage = _fillPercentage
	def fillPercentage_=(perc:Double) {
	  require(perc >= 0 && perc <= 1)
	  _fillPercentage = perc
	}
	
	def draw(x:Int, y:Int, gc:GraphicsContext) {
	  gc.save()
	  gc.setFill(fillColor)
	  val filledHeight = (height-2) * fillPercentage
	  val filledWidth = width
	  gc.fillRect(x, y - filledHeight + height - 1, width, filledHeight)
	  gc.setStroke(borderColor)
	  gc.setLineWidth(1)
	  gc.beginPath()
	  gc.rect(x, y, width, height)
	  gc.closePath()
	  gc.strokePath()
	  gc.restore()
	}
}