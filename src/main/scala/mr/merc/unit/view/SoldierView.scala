package mr.merc.unit.view

import mr.merc.map.hex.Direction
import mr.merc.map.hex.view.TerrainHexView
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import mr.merc.view.move.Movement
import mr.merc.unit.Soldier
import mr.merc.view.move.SoldierMoveMovement
import mr.merc.view.move.MovementList
import scalafx.scene.paint.Color
import scalafx.scene.canvas.GraphicsContext
import mr.merc.unit._
import mr.merc.map.hex.view.TerrainHexFieldView
import mr.merc.map.view.SoldiersDrawer

object SoldierView {
  private[view] val attackDistancePercentage = 0.6

  private[view] def coordsCorrection(dir: Direction, factor: Double): (Int, Int) = {
    val hexField = new TerrainHexField(4, 4, (x, y) => new TerrainHex(x, y, GreenGrass))
    val hexFieldView = new TerrainHexFieldView(hexField, new SoldiersDrawer, factor)
    val center = hexField.hex(1, 1)
    val centerView = new TerrainHexView(center, hexField, hexFieldView, factor)
    val neig = hexField.neighboursWithDirections(1, 1)(dir)
    val neigView = new TerrainHexView(neig, hexField, hexFieldView, factor)

    (neigView.x - centerView.x, neigView.y - centerView.y)
  }

}

class SoldierView(val soldier: Soldier, factor: Double, drawOval: Boolean = true, drawCircleState:Boolean = true) extends AbstractSoldierView(SoldierTypeViewInfo(soldier.soldierType.viewName, soldier.owner.color), factor) {

  private val maxHp = 100
  private val healthBarHeight = Math.min(soldier.soldierType.hp, maxHp) * 2 * TerrainHexView.side(factor) / 3 / maxHp
  private val healthBarWidth = 4

  private val maxXp = 200
  private val xpBarHeight = Math.min(soldier.soldierType.exp, maxXp) * TerrainHexView.side(factor) / maxXp
  private val xpBarWidth = 4

  lazy val sounds = SoldierTypeViewInfo(soldier.soldierType.name).sounds

  val healthBar = new VerticalBarView(healthBarWidth, healthBarHeight, Color.White, Color.Red, hpPercent)
  val xpBar = new VerticalBarView(xpBarWidth, xpBarHeight, Color.White, Color.White, xpPercent)

  def hpPercent = soldier.hp.toDouble / soldier.soldierType.hp
  def xpPercent = soldier.exp.toDouble / soldier.soldierType.exp

  override def drawItself(gc: GraphicsContext, xOffset: Int, yOffset: Int) {
    if (state != DeathState && state != NoState && !state.isInstanceOf[SoldierViewAttackState] && drawOval) {
      drawOvalUnderSoldier(gc, xOffset: Int, yOffset: Int, soldier.owner.color)
    }

    if (state != DeathState && state != NoState && drawCircleState) {
      drawAttackStatusCircleNearSoldier(gc, xOffset, yOffset)
    }

    super.drawItself(gc, xOffset, yOffset)

    if (state != DeathState && state != NoState && hpPercent != 1) {
      healthBar.draw(x + xOffset + 12, y + yOffset + 15, gc)
      //xpBar.draw(x + 6, y + TerrainHexView.Side - xpBarHeight, gc)
    }
  }

  private def drawAttackStatusCircleNearSoldier(gc: GraphicsContext, xOffset: Int, yOffset: Int) {
    gc.save()
    val color = soldier.turnState match {
      case NotHisTurn => Color.Gray
      case HaventMoved | StillCanMove => Color.Green
      case CanntMoveAnyMore => Color.Blue
      case HaveAttacked => Color.Red
    }

    gc.fill = color
    gc.fillOval(x + xOffset + 18, y + yOffset, 8, 8)
    gc.restore()
  }

  def moveMovement(path: List[TerrainHexView], field: TerrainHexFieldView): Movement = {
    val list = path zip path.tail map (p => new SoldierMoveMovement(p._1, p._2, this, field))
    new MovementList(list)
  }

  def refreshBars() {
    if (healthBar.fillPercentage != hpPercent || xpBar.fillPercentage != xpPercent) {
      healthBar.fillPercentage = hpPercent
      //xpBar.fillPercentage = xpPercent
      markAsDirty()
    }
  }
}

