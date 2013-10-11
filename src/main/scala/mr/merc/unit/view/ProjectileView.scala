package mr.merc.unit.view

import mr.merc.image.MImage
import mr.merc.view.move.LinearMovement
import mr.merc.view.move.Movement
import mr.merc.view.Sprite
import mr.merc.view.SpriteState

class ProjectileView(start:List[MImage], move:List[MImage], end:List[MImage], 
    from:(Int, Int), to:(Int, Int), speed:Int) extends Sprite[SpriteState](
        Map((ProjectileStart -> start), (ProjectileMovement -> move), 
            (ProjectileEnd -> end), (ProjectileNotRender -> List(MImage.emptyImage))), 
            ProjectileNotRender, mirroringEnabled = false) {
  
        override def updateTime(time:Int):Int = {
          val result = super.updateTime(time)
          if (result != 0 && index == 0) {
            state match {
              case ProjectileStart => this.state = ProjectileMovement
              case ProjectileMovement => this.state = ProjectileEnd
              case ProjectileEnd => this.state = ProjectileNotRender
              case _ => // do nothing in this case
            }
          }
          
          result
        }
	}

object ProjectileStart extends SpriteState
object ProjectileMovement extends SpriteState
object ProjectileEnd extends SpriteState
object ProjectileNotRender extends SpriteState