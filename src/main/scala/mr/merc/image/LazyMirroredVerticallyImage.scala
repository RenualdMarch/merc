package mr.merc.image

class LazyMirroredVerticallyImage private [image] (origin:MImage, xOffset:Int, yOffset:Int, alpha:Float) extends MImage(xOffset:Int, yOffset:Int, alpha:Float) {
	@volatile
	lazy val image = ImageUtil.mirrorVertically(origin.image)
	
	def imagePath = None
	
	override def changeAlpha(newAlpha:Float) = new LazyMirroredVerticallyImage(origin, xOffset, yOffset, newAlpha)
}