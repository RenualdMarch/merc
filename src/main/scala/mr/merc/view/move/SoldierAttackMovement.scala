package mr.merc.view.move

import mr.merc.map.hex.TerrainHexView
import mr.merc.unit.view.SoldierView
import mr.merc.map.hex.Direction
import mr.merc.map.hex._
import mr.merc.unit.view.SoldierViewAttackState

object SoldierAttackMovement {
  private val frameCount = 20
  val attackSpeed = 100
  
  private [view] def imagesList(imagesCount:Int):List[Int] = {
    val incr = imagesCount * 1000 / frameCount
    val res = for (i <- 0 until imagesCount * 1000 by incr) yield {
      i / 1000
    }
    
    res.toList
  }
}

class SoldierAttackMovement(from:(Int, Int), to:(Int, Int), dir:Direction,
    success:Boolean, soldier:SoldierView, attackNumber:Int) extends Movement {  
    private val state = SoldierViewAttackState(success, dir, attackNumber)
    private val attackImagesSize = soldier.images(state).size
    val frames = SoldierAttackMovement.imagesList(attackImagesSize)
    
    // attack sequence is played
    private val linearMovementToEnemy = new LinearMovement(from._1, from._2, to._1, to._2, SoldierAttackMovement.attackSpeed)
    
    // last frame of attack animation is here
    private val linearMovementFromEnemy = new LinearMovement(to._1, to._2, from._1, from._2, SoldierAttackMovement.attackSpeed)
    
    def start() {
      soldier.animationEnabled = false
      soldier.mirroringEnabled = false
      
      if (dir == SE || dir == NE) {
        soldier.rightDirection = true
      } else if (dir == SW || dir == NW) {
        soldier.rightDirection = false
      }
      
      soldier.x = from._1
      soldier.y = from._2
      
      soldier.state = state
      soldier.index = 0
    }
    
    private def frameListIndex:Int = {
      val i = (linearMovementToEnemy.coveredPart * SoldierAttackMovement.frameCount round)
      if (i >= SoldierAttackMovement.frameCount) {
        (i - 1) toInt
      } else {
        i toInt
      }
    }
    
    def update(time:Int) {
      require(!isOver)
      if (linearMovementToEnemy.isOver) {
        handleMoveFromEnemy(time)
      } else {
        handleMoveToEnemy(time)
      }
      
      if (isOver) {
        changeSoldierConfigurationBack()
      }
    }
    
    private def changeSoldierConfigurationBack() {
      soldier.animationEnabled = true
      soldier.mirroringEnabled = true
    }
    
    private def handleMoveToEnemy(time:Int) {
      linearMovementToEnemy.update(time)
      soldier.x = linearMovementToEnemy.x
      soldier.y = linearMovementToEnemy.y
      soldier.index = frames(frameListIndex)
    }
    
    private def handleMoveFromEnemy(time:Int) {
      linearMovementFromEnemy.update(time)
      soldier.x = linearMovementFromEnemy.x
      soldier.y = linearMovementFromEnemy.y
      soldier.index = attackImagesSize - 1
    }
    
	def isOver = linearMovementToEnemy.isOver && linearMovementFromEnemy.isOver
}