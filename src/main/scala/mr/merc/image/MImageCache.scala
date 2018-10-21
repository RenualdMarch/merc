package mr.merc.image

import scalafx.scene.image.Image
import mr.merc.log.Logging
import mr.merc.util.CacheFactoryMap

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
      new Image(getClass.getResourceAsStream(path))
    } catch {
      case e: Exception =>
        error(s"Failed to load image with path $path", e)
        throw e
    }
  }

  def clear() {
    cache.clear
  }

  private[image] val scaledCache = new CacheFactoryMap[(Image, Double), Image]({
    case (image, factor) =>
      if (factor == 1.0) image
      else ImageUtil.scale(image, factor)
  })

  def scale(image: Image, factor: Double): Image = scaledCache(image, factor)
}