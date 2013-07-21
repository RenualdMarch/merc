package mr.merc.image

class MImageInfo(val path:String, val xOffset:Int = 0, val yOffset:Int = 0, val alpha:Float = 1f) {

  def this() = this("/", 0, 0 , 1f)
  
  def cloneWithAlpha(newAlpha:Float) = new MImageInfo(path, xOffset, yOffset, newAlpha)

  def cloneWithOffset(newOffset:(Int, Int)) = new MImageInfo(path, newOffset._1, newOffset._2, alpha)

  def cloneWithPath(newPath:String) = new MImageInfo(newPath, xOffset, yOffset, alpha)

  override def equals(that:Any):Boolean = {
    that match {
      case it:MImageInfo => it.alpha == alpha &&
        it.xOffset == xOffset && it.yOffset == yOffset && it.path == path
      case _ => false
    }
  }

  override def hashCode:Int = xOffset + 31 * yOffset + path.hashCode

  def buildImage:MImage = MImage(path, xOffset, yOffset, alpha)
}