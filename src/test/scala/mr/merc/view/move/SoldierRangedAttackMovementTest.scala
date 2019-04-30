package mr.merc.view.move

import org.scalatest.FunSuite
import mr.merc.unit.SoldierType
import mr.merc.unit.Attack
import mr.merc.unit.Impact
import mr.merc.unit.view.SoldierView
import mr.merc.unit.Soldier
import mr.merc.players.Player
import mr.merc.map.hex.NE
import mr.merc.unit.view.ProjectileStart
import mr.merc.unit.view.ProjectileMovement
import mr.merc.unit.view.ProjectileEnd
import mr.merc.unit.view.ProjectileNotRender
import mr.merc.unit.AttackResult
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.hex.view.TerrainHexView
import mr.merc.map.hex.view.TerrainHexFieldView
import mr.merc.map.view.SoldiersDrawer

class SoldierRangedAttackMovementTest extends FunSuite {
  val simpleSoldierType = new SoldierType("testSoldier2", 1, 20, 10, 5, 1,
    List(Attack(0, 5, 2, Impact, true)), Map(), Map(), Map(), Set(),"testSoldier2")

  // FIXME unignore me
  ignore("simple attack") {
    val attackerSoldier = new Soldier("1", simpleSoldierType, Player("1"))
    val defenderSoldier = new Soldier("2", simpleSoldierType, Player("2"))
    val attackerView = new SoldierView(attackerSoldier, 1.0)
    val defenderView = new SoldierView(defenderSoldier, 1.0)

    val result = AttackResult(true, attackerSoldier, defenderSoldier, simpleSoldierType.attacks(0), false, 0, 0)
    val field = new TerrainHexField(10, 10, TerrainHex.grassInit)
    val fieldView = new TerrainHexFieldView(field, new SoldiersDrawer(), 1.0)
    val from = new TerrainHexView(field.hex(0, 0), field, fieldView, 1.0) {
      override def coords = (10, 20)
    }

    val to = new TerrainHexView(field.hex(1, 0), field, fieldView, 1.0) {
      override def coords = (110, 20)
    }

    val movement = new SoldierRangedAttackMovement(from, to, NE,
      attackerView, defenderView, result, fieldView)
    movement.start()
    assert(movement.projectileView.state === ProjectileNotRender)
    assert(attackerView.index === 0)
    movement.update(attackerView.duration)
    assert(attackerView.index === 1)
    movement.update(attackerView.duration)
    assert(attackerView.index === 2)
    movement.update(attackerView.duration)
    assert(attackerView.index === 3)
    movement.update(attackerView.duration)
    assert(attackerView.index === 0)
    assert(movement.projectileView.state === ProjectileStart)
    movement.update(100000)
    assert(attackerView.index === 0)
    assert(movement.projectileView.state === ProjectileMovement)
    movement.update(100000)
    assert(attackerView.index === 0)
    assert(movement.projectileView.state === ProjectileEnd)
    movement.update(100000)
    assert(attackerView.index === 0)
    assert(movement.projectileView.state === ProjectileNotRender)
    assert(movement.isOver)
  }
}