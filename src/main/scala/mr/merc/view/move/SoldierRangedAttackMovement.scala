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
import mr.merc.unit.view.SoldierTypeViewInfo
import mr.merc.unit.sound.AttackSound
import mr.merc.unit.sound.PainSound
import mr.merc.unit.sound.AttackSound
import mr.merc.map.hex.view.TerrainHexView
import mr.merc.map.hex.view.TerrainHexFieldView

class SoldierRangedAttackMovement(val fromHex: TerrainHexView, val toHex: TerrainHexView, val dir: Direction,
  val attacker: SoldierView, val defender: SoldierView, result: AttackResult, field: TerrainHexFieldView) extends Movement {

  private val factor = fromHex.factor
  val from = fromHex.coords
  val to = toHex.coords
  private val attackViews = SoldierTypeViewInfo(attacker.soldier.soldierType.name).attacks
  private val projectileName = attackViews.find(_.index == result.attackIndex).get.projectileName(result.success).get
  val projectileView = Projectile(projectileName).buildView(dir, from, to, factor)
  private var attackerFinishedHisThrowingMove = false
  private var painSoundPlayed = false

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

  override def start() {
    super.start()
    val direction = field.neighboursWithDirections(fromHex).toList.map { case (k, v) => v -> k }.toMap.apply(toHex)
    attacker.lookAtDirection(direction)
    (damageNumberMovement ++ drainNumberMovement).foreach(_.start())
    attacker.state = SoldierViewAttackState(result.success, dir, result.attackIndex)
    attacker.sounds.get(AttackSound(result.attackIndex, result.success)).foreach(_.play)
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

    if (Set(ProjectileNotRender, ProjectileEnd).contains(projectileView.state) &&
      !painSoundPlayed && attackerFinishedHisThrowingMove && result.success) {
      defender.sounds.get(PainSound).foreach(_.play)
      painSoundPlayed = true
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

  override def drawables = List(defender, attacker, projectileView) ++ numberMovements
}