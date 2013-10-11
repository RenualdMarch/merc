package mr.merc.image

class LazyMirroredHorizontallyImage private [image] (origin:MImage, xOffset:Int, yOffset:Int, alpha:Float) extends MImage(xOffset:Int, yOffset:Int, alpha:Float) {
	lazy val image = ImageUtil.mirrorHorizontally(origin.image)
	
	def imagePath = None
	
	override def changeAlpha(newAlpha:Float) = new LazyMirroredHorizontallyImage(origin, xOffset, yOffset, newAlpha)

}