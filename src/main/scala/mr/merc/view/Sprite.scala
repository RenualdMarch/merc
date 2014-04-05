package mr.merc.view

import scalafx.scene.canvas.GraphicsContext
import mr.merc.image.MImage
import scalafx.geometry.Rectangle2D
import mr.merc.map.hex._

object Sprite {

  def apply(images: Map[SpriteState, List[MImage]], initialState: SpriteState): Sprite[SpriteState] = {
    new Sprite[SpriteState](images, initialState)
  }

  def apply(list: List[MImage]): Sprite[SpriteState] = apply(Map[SpriteState, List[MImage]](DefaultState -> list), DefaultState)

  def apply(image: MImage): Sprite[SpriteState] = apply(List(image))
}

class Sprite[T <: SpriteState](val images: Map[T, List[MImage]], private var _state: T,
  var mirroringEnabled: Boolean = true, var animationEnabled: Boolean = true) extends Drawable {
  require(images.keys.exists(_ == _state), "Initial state isn't present in map!")
  require(!images.values.exists(_.size == 0), s"State ${images.find(_._2.size == 0).get._1} with zero images")

  private var _time = 0
  private var _index = 0
  var duration = 100
  private var _x = 0
  private var _y = 0

  var rightDirection = true

  var dirtyRect: Option[Rectangle2D] = None

  def lookAtDirection(direction: Direction) {
    if (direction == NE || direction == SE) {
      rightDirection = true
    } else if (direction == NW || direction == SW) {
      rightDirection = false
    }
  }

  def calculateDirtyRect = centered match {
    case Some(size) => currentImage.centeredRect(x, y, size, size)
    case None => new Rectangle2D(x, y, currentImage.width, currentImage.height)
  }

  def viewRect = calculateDirtyRect

  def markAsDirty() {
    dirtyRect = Some(calculateDirtyRect)
  }

  def y = _y
  private def y_=(i: Int) {
    _y = i
  }

  def coords = (x, y)

  def coords_=(c: (Int, Int)) {
    markAsDirty()
    x = c._1
    y = c._2
  }

  def x = _x
  private def x_=(i: Int) {
    if (mirroringEnabled) {
      val oldX = _x
      _x = i
      if (oldX > x) {
        rightDirection = false
      } else if (oldX < x) {
        rightDirection = true
      }
    } else {
      _x = i
    }
  }

  def time = _time

  def index = _index

  def index_=(i: Int) {
    require(i >= 0, "Index cann't be less then 0")
    require(i < images(state).size, s"Index $i is bigger than size ${images(state).size} for state $state")
    _index = i
    _time = 0
  }

  def state = _state

  def state_=(st: T) {
    require(images.keys.exists(_ == st), s"State $st isn't present in map!")
    if (st != state) {
      markAsDirty()
      _state = st
      _time = 0
      _index = 0
    }
  }

  private def currentImage = images(state)(index)

  var centered: Option[Int] = None
  def drawItself(gc: GraphicsContext, xOffset: Int, yOffset: Int) {
    centered match {
      case Some(side) => imageToDraw.drawCenteredImage(gc, x + xOffset, y + yOffset, side, side)
      case None => imageToDraw.drawImage(gc, x + xOffset, y + yOffset)
    }
  }

  def imageToDraw = if (rightDirection) {
    currentImage
  } else {
    currentImage.mirrorVertically
  }

  def updateTime(delta: Int): Int = {
    _time += delta
    val increase = _time / duration

    if (animationEnabled) {
      increaseIndex(increase)
    }

    _time -= increase * duration
    increase
  }

  private def increaseIndex(increase: Int) {
    val frames = images(state).size
    if (frames != 1) {
      markAsDirty()
    }
    _index = (index + increase) % frames
  }
}

object DefaultState extends SpriteState