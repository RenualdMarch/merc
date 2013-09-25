package mr.merc.image

import scalafx.scene.image.Image


 class EagerMImage private [image] (val image:Image, xOffset:Int, yOffset:Int, alpha:Float)
  extends MImage(xOffset, yOffset, alpha) {

  def imagePath = None
  
  override def changeAlpha(newAlpha:Float) = new EagerMImage(image, xOffset, yOffset, newAlpha)
}