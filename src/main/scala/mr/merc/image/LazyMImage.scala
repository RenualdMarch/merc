package mr.merc.image

private [image] class LazyMImage(path:String, xOffset:Int, yOffset:Int, alpha:Float) extends MImage(xOffset, yOffset, alpha){
  lazy val image = MImageCache.get(path)

  def imagePath = Some(path)
}
