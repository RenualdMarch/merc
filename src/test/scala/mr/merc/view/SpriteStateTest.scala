package mr.merc.view

import org.scalatest.FunSuite
import mr.merc.image.MImage
import org.scalatest.BeforeAndAfter

class SpriteStateTest extends FunSuite with BeforeAndAfter {
  object TestState1 extends SpriteState
  object TestState2 extends SpriteState
  val image1 = MImage("/testImages/image1.png")
  val image2 = MImage("/testImages/image2.png")
  val image3 = MImage("/testImages/image3.png")
  val image4 = MImage("/testImages/image4.png")
  val images = Map((TestState1 -> List(image1)), (TestState2 -> List(image2, image3, image4)))
  var sprite: Sprite[SpriteState] = _

  before {
    sprite = Sprite(images, TestState2)
    sprite.duration = 50
  }

  test("initial state") {
    assert(sprite.state === TestState2)
  }

  test("next frame") {
    sprite.updateTime(30)
    assert(sprite.index === 0)
    assert(sprite.updateTime(20) === 1)
    assert(sprite.index === 1)
  }

  test("two frames at once") {
    assert(sprite.updateTime(100) === 2)
    assert(sprite.index === 2)
  }

  test("move through frame cycle") {
    assert(sprite.updateTime(60) === 1)
    assert(sprite.index === 1)
    assert(sprite.updateTime(140) === 3)
    assert(sprite.index === 1)
  }

  test("change state") {
    sprite.state = TestState1
    assert(sprite.index === 0)
    assert(sprite.state === TestState1)
  }

  test("one frame") {
    sprite.state = TestState1
    assert(sprite.index === 0)
    sprite.updateTime(50)
    assert(sprite.index === 0)
  }

  test("change index") {
    sprite.updateTime(10)
    sprite.index = 1
    assert(sprite.time === 0)
  }
}