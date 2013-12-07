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

class SoldierRangedAttackMovement(val from: (Int, Int), val to: (Int, Int), val dir: Direction,
  val success: Boolean, val attacker: SoldierView, val defender: SoldierView, val attackNumber: Int) extends Movement {
  private val projectileName = attacker.soldier.soldierType.attacks(attackNumber).projectileName(success).get
  val projectileView = Projectile(projectileName).buildView(dir, from, to)
  private var attackerFinishedHisThrowingMove = false

  override def start() {
    super.start()
    attacker.state = SoldierViewAttackState(success, dir, attackNumber)
  }

  def update(time: Int) = {
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
  }

  def isOver = projectileView.state == ProjectileNotRender && attackerFinishedHisThrowingMove

  // who is first should be rendered first
  override def drawables = List(defender, attacker, projectileView)
}