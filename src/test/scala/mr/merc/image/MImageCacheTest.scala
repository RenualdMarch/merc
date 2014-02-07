package mr.merc.image

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

class MImageCacheTest extends FunSuite with BeforeAndAfter {

  before {
    MImageCache.clear();
  }

  test("can load image") {
    val image = MImageCache.get("/testImages/testImage.png")
    assert(image.width.value === 97)
    assert(image.height.value === 19)
  }

  test("doesn't load image for the second time") {
    val image1 = MImageCache.get("/testImages/testImage.png")
    val image2 = MImageCache.get("/testImages/testImage.png")
    assert(image1 === image2)
    assert(MImageCache.cache.size === 1)
  }

  test("throw exception when image doesn't exist") {
    intercept[NullPointerException] {
      val image = MImageCache.get("/testImages/noExistingImage.png")
    }
  }
}