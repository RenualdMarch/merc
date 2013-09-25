package mr.merc.image

class LazyMirroredImage private [image] (origin:MImage, xOffset:Int, yOffset:Int, alpha:Float) extends MImage(xOffset:Int, yOffset:Int, alpha:Float) {
	lazy val image = ImageUtil.mirror(origin.image)
	
	def imagePath = None
	
	override def changeAlpha(newAlpha:Float) = new LazyMirroredImage(origin, xOffset, yOffset, newAlpha)
}