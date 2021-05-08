package mr.merc.image

class LazyMImage private [image] (path:String, xOffset:Int, yOffset:Int, alpha:Float) extends MImage(xOffset, yOffset, alpha){
  @volatile
  lazy val image = MImageCache.get(path)

  def imagePath = Some(path)
  
  override def changeAlpha(newAlpha:Float) = new LazyMImage(path, xOffset, yOffset, newAlpha)
}
