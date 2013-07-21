package mr.merc.image

import scalafx.scene.image.Image


private [image] class EagerMImage(val image:Image, xOffset:Int, yOffset:Int, alpha:Float)
  extends MImage(xOffset, yOffset, alpha) {

  def imagePath = None
}