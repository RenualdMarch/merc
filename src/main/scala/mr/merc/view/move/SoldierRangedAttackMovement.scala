package mr.merc.view.move

import mr.merc.map.hex.Direction
import mr.merc.unit.view.SoldierView
import mr.merc.view.Drawable
import mr.merc.unit.view.Projectile
import mr.merc.unit.view.ProjectileNotRender
import mr.merc.unit.view.ProjectileStart
import java.util.Date
import mr.merc.unit.view.StandState
import mr.merc.unit.view.SoldierViewAttackState
import mr.merc.unit.AttackResult
import mr.merc.unit.view.ProjectileEnd

class SoldierRangedAttackMovement(val from: (Int, Int), val to: (Int, Int), val dir: Direction,
  val attacker: SoldierView, val defender: SoldierView, result: AttackResult) extends Movement {
  private val projectileName = attacker.soldier.soldierType.attacks(result.attackIndex).projectileName(result.success).get
  val projectileView = Projectile(projectileName).buildView(dir, from, to)
  private var attackerFinishedHisThrowingMove = false

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
  (damageNumberMovement ++ drainNumberMovement).foreach(_.start())

  override def start() {
    super.start()
    attacker.state = SoldierViewAttackState(result.success, dir, result.attackIndex)
  }

  override def update(time: Int) = {
    super.update(time)
    if (attackerFinishedHisThrowingMove) {
      projectileView.updateTime(time)
    } else {
      val indexChanged = attacker.updateTime(time)
      if (indexChanged > 0 && attacker.index == 0) {
        attackerFinishedHisThrowingMove = true
        attacker.state = StandState
        projectileView.state = ProjectileStart
      }
    }

    if (!numbersAreOver) {
      numberMovements.foreach(_.update(time))
    }
  }

  def isOver = projectileView.state == ProjectileNotRender && attackerFinishedHisThrowingMove && numbersAreOver

  private def numberMovements = if ((projectileView.state == ProjectileEnd || projectileView.state == ProjectileNotRender) && attackerFinishedHisThrowingMove) {
    damageNumberMovement ++ drainNumberMovement
  } else {
    Nil
  }

  private def numbersAreOver = damageNumberMovement.map(_.isOver).getOrElse(true) &&
    drainNumberMovement.map(_.isOver).getOrElse(true)

  // who is first should be rendered first
  override def drawables = List(defender, attacker, projectileView) ++ numberMovements
}