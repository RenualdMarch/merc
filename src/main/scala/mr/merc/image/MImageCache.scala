package mr.merc.image

import scalafx.scene.image.Image
import java.io.File
import mr.merc.log.Logging

private[image] object MImageCache extends Logging {
  val cache = collection.mutable.Map[String, Image]()

  def get(path: String): Image = {
    if (cache.contains(path)) {
      cache(path)
    } else {
      val image = loadImage(path)
      cache += (path -> image)
      image
    }
  }

  def loadImage(path: String) = {
    try {
      new Image(getClass().getResourceAsStream(path))
    } catch {
      case e: Exception =>
        error(s"Failed to load image with path $path", e)
        throw e
    }
  }

  def clear() {
    cache.clear
  }
}