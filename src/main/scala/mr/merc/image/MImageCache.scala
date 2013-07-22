package mr.merc.image

import scalafx.scene.image.Image
import java.io.File

private [image] object MImageCache {
  val cache = collection.mutable.Map[String, Image]()

  def get(path:String):Image = {
    if (cache.contains(path)) {
      cache(path)
    } else {
      val image = loadImage(path)
      cache += (path -> image)
      image
    }
  }

  def loadImage(path:String):Image = {
    require(Option(getClass().getResource(path)).isDefined, s"File with path $path doesn't exist")
    new Image(getClass().getResourceAsStream(path))
  }
  
  def clear() {
    cache.clear
  }
}