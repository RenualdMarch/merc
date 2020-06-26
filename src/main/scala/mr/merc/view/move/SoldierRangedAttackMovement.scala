package mr.merc.view.move

import mr.merc.map.hex.Direction
import mr.merc.unit.view.{DefenceState, Projectile, ProjectileEnd, ProjectileNotRender, ProjectileStart, SoldierTypeViewInfo, SoldierView, SoldierViewAttackState, StandState}

import mr.merc.unit.AttackResult
import mr.merc.unit.sound.PainSound
import mr.merc.unit.sound.AttackSound
import mr.merc.map.hex.view.TerrainHexView
import mr.merc.map.hex.view.TerrainHexFieldView

class SoldierRangedAttackMovement(val fromHex: TerrainHexView, val toHex: TerrainHexView, val dir: Direction,
  val attacker: SoldierView, val defender: SoldierView, result: AttackResult, field: TerrainHexFieldView) extends Movement {

  private val factor = fromHex.factor
  val from = fromHex.coords
  val to = toHex.coords
  private val attackViews = SoldierTypeViewInfo(attacker.soldier.soldierType.viewName).attacks
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

  private def changeHp(): Unit = {
    if (result.success) {
      attacker.viewHp += result.drained
      defender.viewHp -= result.damage
    }
  }

  private val hpChangeMovement = new MomentaryMovement(changeHp())

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
        attacker.state = DefenceState
        projectileView.state = ProjectileStart
      }
    }

    if (Set(ProjectileNotRender, ProjectileEnd).contains(projectileView.state) &&
      !painSoundPlayed && attackerFinishedHisThrowingMove && result.success) {
      defender.sounds.get(PainSound).foreach(_.play())
      painSoundPlayed = true
    }

    if (!numbersAreOver) {
      numberMovements.foreach(_.update(time))
      if (!hpChangeMovement.isOver) {
        hpChangeMovement.start()
      }
    }
  }

  def isOver = projectileView.state == ProjectileNotRender && attackerFinishedHisThrowingMove && numbersAreOver

  private def numberMovements = if ((projectileView.state == ProjectileEnd || projectileView.state == ProjectileNotRender) && attackerFinishedHisThrowingMove) {
    damageNumberMovement ++ drainNumberMovement
  } else {
    Nil
  }

  private def numbersAreOver = damageNumberMovement.forall(_.isOver) &&
    drainNumberMovement.forall(_.isOver)

  override def drawables = List(defender, attacker, projectileView) ++ numberMovements
}