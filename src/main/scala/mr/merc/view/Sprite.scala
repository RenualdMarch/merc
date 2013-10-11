package mr.merc.view

import scalafx.scene.canvas.GraphicsContext
import mr.merc.image.MImage

object Sprite {
  def apply(images:Map[SpriteState, List[MImage]], initialState:SpriteState):Sprite[SpriteState] = {
    new Sprite[SpriteState](images, initialState)
  }
  
  def apply(list:List[MImage]):Sprite[SpriteState] = apply(Map[SpriteState, List[MImage]](DefaultState -> list), DefaultState)
  
  def apply(image:MImage):Sprite[SpriteState] = apply(List(image))
}

class Sprite[T <: SpriteState](val images:Map[T, List[MImage]], private var _state:T, 
			var mirroringEnabled:Boolean = true, var animationEnabled:Boolean = true) extends Drawable {
	require(images.keys.exists(_ == _state), "Initial state isn't present in map!")
	require(!images.values.exists(_.size == 0), "There are states with zero images")
	
	private var _time = 0
	private var _index = 0
	var duration = 100
	private var _x = 0
	var y = 0
	var rightDirection = true
	
	def x = _x
	
	def x_=(i:Int) {
	  if (mirroringEnabled) {
		  val oldX = _x
		  _x = i
		  if (oldX > x) {
		    rightDirection = false
		  } else {
		    rightDirection = true	    
		  }
	  } else {
	    _x = i
	  }
	}
	
	def time = _time
	
	def index = _index
	
	def index_=(i:Int) {
	  require(i >= 0, "Index cann't be less then 0")
	  require(i < images(state).size, "Index bigger than size")
	  _index = i
	  _time = 0
	}
	
	def state = _state
	
	def state_=(st:T) {
	  require(images.keys.exists(_ == st), "State isn't present in map!")
	  _state = st
	  _time = 0
	}
  
    private def currentImage = images(state)(index)
    
    def drawItself(gc:GraphicsContext) {
      if (rightDirection) {
    	currentImage.drawImage(gc, x, y)
      } else {
        currentImage.mirrorVertically.drawImage(gc, x, y)
      }
    }
	
    def updateTime(delta:Int):Int = {
      _time += delta
      val increase = _time / duration
      
      if (animationEnabled) {
    	  increaseIndex(increase)
      }
      
      _time -= increase * duration
      increase
    }
    
    private def increaseIndex(increase:Int) {
      val frames = images(state).size
      _index = (index + increase) % frames
    }
}

object DefaultState extends SpriteState