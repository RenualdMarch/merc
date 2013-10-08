package mr.merc.view.move

import mr.merc.map.hex.view.TerrainHexView
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
    success:Boolean, attacker:SoldierView, defender:SoldierView, attackNumber:Int) extends Movement {  
    private val state = SoldierViewAttackState(success, dir, attackNumber)
    private val attackImagesSize = attacker.images(state).size
    val frames = SoldierAttackMovement.imagesList(attackImagesSize)
    
    // attack sequence is played
    private val linearMovementToEnemy = new LinearMovement(from._1, from._2, to._1, to._2, SoldierAttackMovement.attackSpeed)
    
    // last frame of attack animation is here
    private val linearMovementFromEnemy = new LinearMovement(to._1, to._2, from._1, from._2, SoldierAttackMovement.attackSpeed)
    
    def start() {
      attacker.animationEnabled = false
      attacker.mirroringEnabled = false
      
      if (dir == SE || dir == NE) {
        attacker.rightDirection = true
      } else if (dir == SW || dir == NW) {
        attacker.rightDirection = false
      }
      
      attacker.x = from._1
      attacker.y = from._2
      
      attacker.state = state
      attacker.index = 0
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
      attacker.animationEnabled = true
      attacker.mirroringEnabled = true
    }
    
    private def handleMoveToEnemy(time:Int) {
      linearMovementToEnemy.update(time)
      attacker.x = linearMovementToEnemy.x
      attacker.y = linearMovementToEnemy.y
      attacker.index = frames(frameListIndex)
    }
    
    private def handleMoveFromEnemy(time:Int) {
      linearMovementFromEnemy.update(time)
      attacker.x = linearMovementFromEnemy.x
      attacker.y = linearMovementFromEnemy.y
      attacker.index = attackImagesSize - 1
    }
    
  
    override def soldiers = List(defender, attacker)
    
	def isOver = linearMovementToEnemy.isOver && linearMovementFromEnemy.isOver
}