package mr.merc.unit.view

import mr.merc.view.SpriteState
import mr.merc.view.Sprite
import mr.merc.image.MImage
import mr.merc.map.hex.Direction
import mr.merc.map.hex.HexField
import mr.merc.map.hex.Hex
import mr.merc.map.hex.view.TerrainHexView
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import mr.merc.view.move.Movement
import mr.merc.unit.Soldier
import mr.merc.view.move.SoldierMoveMovement
import mr.merc.view.move.MovementList
import mr.merc.view.move.SoldierAttackMovement
import mr.merc.unit.AttackResult
import scalafx.scene.paint.Color
import mr.merc.unit.SoldierType
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.shape.Ellipse
import mr.merc.unit._

object SoldierView {
  private[view] val attackDistancePercentage = 0.6

  private[view] def coordsCorrection(dir: Direction): (Int, Int) = {
    val hexField = new TerrainHexField(4, 4, (x, y) => new TerrainHex(x, y, Grass))
    val center = hexField.hex(1, 1)
    val centerView = new TerrainHexView(center, hexField)
    val neig = hexField.neighboursWithDirections(1, 1)(dir)
    val neigView = new TerrainHexView(neig, hexField)

    (neigView.x - centerView.x, neigView.y - centerView.y)
  }

}

class SoldierView(val soldier: Soldier) extends Sprite[SoldierViewState](SoldierTypeViewInfo(soldier.soldierType.name, soldier.player.color).images, StandState) {
  private val maxHp = 100
  private val healthBarHeight = Math.min(soldier.soldierType.hp, maxHp) * TerrainHexView.Side / maxHp
  private val healthBarWidth = 4

  private val maxXp = 200
  private val xpBarHeight = Math.min(soldier.soldierType.exp, maxXp) * TerrainHexView.Side / maxXp
  private val xpBarWidth = 4

  lazy val sounds = SoldierTypeViewInfo(soldier.soldierType.name).sounds

  val healthBar = new VerticalBarView(healthBarWidth, healthBarHeight, Color.WHITE, Color.RED, hpPercent)
  val xpBar = new VerticalBarView(xpBarWidth, xpBarHeight, Color.WHITE, Color.WHITE, xpPercent)

  def hpPercent = soldier.hp.toDouble / soldier.soldierType.hp
  def xpPercent = soldier.exp.toDouble / soldier.soldierType.exp

  override def drawItself(gc: GraphicsContext) {
    if (state != DeathState && state != NoState && !state.isInstanceOf[SoldierViewAttackState]) {
      drawOvalUnderSoldier(gc)
    }

    if (state != DeathState && state != NoState) {
      drawAttackStatusCircleNearSoldier(gc)
    }

    super.drawItself(gc)

    if (state != DeathState && state != NoState) {
      healthBar.draw(x, y + TerrainHexView.Side - healthBarHeight, gc)
      xpBar.draw(x + 6, y + TerrainHexView.Side - xpBarHeight, gc)
    }
  }

  override def updateTime(delta: Int): Int = {
    val result = super.updateTime(delta)
    if (result > 0 && index == 0) {
      if (state == DeathState) {
        state = NoState
      } else if (state == IdleState) {
        state = StandState
      }
    }
    result
  }

  private def drawOvalUnderSoldier(gc: GraphicsContext) {
    gc.save()
    gc.fill = soldier.player.color
    gc.globalAlpha = 0.2
    gc.fillOval(x + 9, y + 48, 48, 24)
    gc.globalAlpha = 1
    gc.stroke = soldier.player.color
    gc.strokeOval(x + 9, y + 48, 48, 24)
    gc.restore()
  }

  private def drawAttackStatusCircleNearSoldier(gc: GraphicsContext) {
    gc.save()
    val color = soldier.turnState match {
      case NotHisTurn => Color.GRAY
      case HaventMoved | StillCanMove => Color.GREEN
      case CanntMoveAnyMore => Color.BLUE
      case HaveAttacked => Color.RED
    }

    gc.fill = color
    gc.fillOval(x, y, 12, 12)
    gc.restore()

  }

  def moveMovement(path: List[TerrainHexView]): Movement = {
    val list = path zip path.tail map (p => new SoldierMoveMovement(p._1, p._2, this))
    new MovementList(list)
  }

  def refreshBars() {
    healthBar.fillPercentage = hpPercent
    xpBar.fillPercentage = xpPercent
  }
}

