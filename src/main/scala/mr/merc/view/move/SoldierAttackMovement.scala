package mr.merc.view.move

import mr.merc.map.hex.view.TerrainHexView
import mr.merc.unit.view.SoldierView
import mr.merc.map.hex.Direction
import mr.merc.map.hex._
import mr.merc.unit.view.SoldierViewAttackState
import mr.merc.unit.view.StandState
import mr.merc.unit.AttackResult
import mr.merc.unit.sound.AttackSound
import mr.merc.unit.sound.PainSound

object SoldierAttackMovement {
  private val frameCount = 20

  private[view] def imagesList(imagesCount: Int): List[Int] = {
    val incr = imagesCount * 1000 / frameCount
    val res = for (i <- 0 until imagesCount * 1000 by incr) yield {
      i / 1000
    }

    res.toList
  }
}

class SoldierAttackMovement(val from: (Int, Int), val to: (Int, Int), val dir: Direction,
  val attacker: SoldierView, val defender: SoldierView,
  val result: AttackResult, val attackSpeed: Int = 150) extends Movement {

  private var defenderPainSoundPlayed = false
  private val attackMovementPercentage = 0.7
  private val state = SoldierViewAttackState(result.success, dir, result.attackIndex)
  private val attackImagesSize = attacker.images(state).size
  val frames = SoldierAttackMovement.imagesList(attackImagesSize)

  private val damageNumberMovement: Option[ShowingNumberDrawerMovement] = if (result.success) {
    Some(ShowingNumberDrawerMovement.damage(to._1, to._2, result.damage))
  } else {
    None
  }

  private val drainNumberMovement: Option[ShowingNumberDrawerMovement] = if (result.drained != 0) {
    Some(ShowingNumberDrawerMovement.damage(from._1, from._2, result.drained))
  } else {
    None
  }

  // attack sequence is played
  private val linearMovementToEnemy = new LinearMovement(from._1, from._2, to._1, to._2, attackSpeed, attackMovementPercentage)
  linearMovementToEnemy.start()
  private val dest = linearMovementToEnemy.destination
  // last frame of attack animation is here
  private val linearMovementFromEnemy = new LinearMovement(dest._1, dest._2, from._1, from._2, attackSpeed)
  linearMovementFromEnemy.start()
  (damageNumberMovement ++ drainNumberMovement).foreach(_.start())

  override def start() {
    super.start()
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

    attacker.sounds.get(AttackSound(result.attackIndex, result.success)).foreach(_.play)
  }

  private def frameListIndex: Int = {
    val i = (linearMovementToEnemy.coveredPart * SoldierAttackMovement.frameCount round)
    if (i >= SoldierAttackMovement.frameCount) {
      (i - 1) toInt
    } else {
      i toInt
    }
  }

  override def update(time: Int) {
    super.update(time)
    if (linearMovementToEnemy.isOver) {
      handleMoveFromEnemy(time)
    } else {
      handleMoveToEnemy(time)
    }

    if (isOver) {
      changeSoldierConfigurationBack()
    }

    if (result.success && !defenderPainSoundPlayed && linearMovementToEnemy.isOver) {
      defenderPainSoundPlayed = true
      defender.sounds.get(PainSound).foreach(_.play)
    }
  }

  private def changeSoldierConfigurationBack() {
    attacker.animationEnabled = true
    attacker.mirroringEnabled = true
    attacker.state = StandState
  }

  private def handleMoveToEnemy(time: Int) {
    linearMovementToEnemy.update(time)
    attacker.x = linearMovementToEnemy.x
    attacker.y = linearMovementToEnemy.y
    attacker.index = frames(frameListIndex)
  }

  private def handleMoveFromEnemy(time: Int) {
    if (!linearMovementFromEnemy.isOver) {
      linearMovementFromEnemy.update(time)
      attacker.x = linearMovementFromEnemy.x
      attacker.y = linearMovementFromEnemy.y
      attacker.index = attackImagesSize - 1
    }

    if (!numbersAreOver) {
      numberMovements.foreach(_.update(time))
    }
  }

  private def numberMovements = if (linearMovementToEnemy.isOver) {
    damageNumberMovement ++ drainNumberMovement
  } else {
    Nil
  }

  private def numbersAreOver = damageNumberMovement.map(_.isOver).getOrElse(true) &&
    drainNumberMovement.map(_.isOver).getOrElse(true)

  override def drawables = List(defender, attacker) ++ numberMovements

  def isOver = linearMovementToEnemy.isOver && linearMovementFromEnemy.isOver && numbersAreOver
}