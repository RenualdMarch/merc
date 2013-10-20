package mr.merc.unit.view

import mr.merc.image.MImage
import mr.merc.view.move.LinearMovement
import mr.merc.view.move.Movement
import mr.merc.view.Sprite
import mr.merc.view.SpriteState

class ProjectileView(start:List[MImage], move:List[MImage], end:List[MImage], 
    from:(Int, Int), to:(Int, Int), speed:Int) extends Sprite[ProjectileState](
        Map((ProjectileStart -> start), (ProjectileMovement -> move), 
            (ProjectileEnd -> end), (ProjectileNotRender -> List(MImage.emptyImage))), 
            ProjectileNotRender, mirroringEnabled = false) {
		val movement = new LinearMovement(from._1, from._2, to._1, to._2, speed)
		movement.start()
		
        override def updateTime(time:Int):Int = {
          val result = super.updateTime(time)
          
          if (state == ProjectileMovement) {            
             movement.update(time)
             x = movement.x
             y = movement.y
          }
          
          if (result != 0 && index == 0) {
            state match {
              case ProjectileStart => this.state = ProjectileMovement
              case ProjectileMovement => {
                if (movement.isOver) {
                  this.state = ProjectileEnd
                }
              }
              case ProjectileEnd => this.state = ProjectileNotRender
              case _ => // do nothing in this case
            }
          }
          
          result
        }
	}

sealed trait ProjectileState extends SpriteState
object ProjectileStart extends ProjectileState
object ProjectileMovement extends ProjectileState
object ProjectileEnd extends ProjectileState
object ProjectileNotRender extends ProjectileState