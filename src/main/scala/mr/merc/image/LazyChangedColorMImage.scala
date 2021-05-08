package mr.merc.image

import scalafx.scene.paint.Color
import scalafx.scene.image.Image

class LazyChangedColorMImage private [image]  (color:Color, parent:MImage, xOffset:Int, yOffset:Int, alpha:Float) extends MImage(xOffset, yOffset, alpha){
	@volatile
	lazy val image = SoldierColorTransformer.transformImage(parent.image, color)
	val imagePath = None
	def changeAlpha(newAlpha:Float) = new LazyChangedColorMImage(color, parent, xOffset, yOffset, newAlpha)
}