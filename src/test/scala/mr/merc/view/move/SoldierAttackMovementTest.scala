package mr.merc.view.move

import org.scalatest.FunSuite
import mr.merc.unit.{AttackResult, Soldier, SoldierType, TestUtil}
import mr.merc.unit.view.{DefenceState, SoldierView, SoldierViewAttackState, StandState}
import mr.merc.map.hex.NE
import mr.merc.players.Player
import mr.merc.map.hex.view.TerrainHexView
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.hex.view.TerrainHexFieldView
import org.scalatest.BeforeAndAfter
import mr.merc.view.Sprite
import mr.merc.map.view.SoldiersDrawer

class SoldierAttackMovementTest extends FunSuite with BeforeAndAfter {

  test("imagesList") {
    assert(SoldierAttackMovement.imagesList(5) === List(0, 0, 0, 0, 1, 1, 1, 1,
      2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4))
    assert(SoldierAttackMovement.imagesList(4) === List(0, 0, 0, 0, 0, 1, 1, 1, 1, 1,
      2, 2, 2, 2, 2, 3, 3, 3, 3, 3))
  }

  test("animation") {
    val soldier = new Soldier("1", TestUtil.testSoldierType2, Player("1"))
    val enemy = new Soldier("2", TestUtil.testSoldierType2, Player("2"))
    val soldierView = new SoldierView(soldier, 1.0)
    val enemyView = new SoldierView(enemy, 1.0)
    val direction = NE

    val field = new TerrainHexField(10, 10, TerrainHex.grassInit)
    val fieldView = new TerrainHexFieldView(field, new SoldiersDrawer(), 1.0)
    val from = new TerrainHexView(field.hex(0, 0), field, fieldView, 1.0) {
      override def coords = (0, 10)
    }

    val to = new TerrainHexView(field.hex(0, 1), field, fieldView, 1.0) {
      override def coords = (300, 410)
    }

    // distance is 500, speed is 100, time is 5 sec
    val result = AttackResult(true, soldier, enemy, soldier.soldierType.attacks(0), true, 10, 0)
    val movement = new SoldierAttackMovement(from, to, direction, // from, to
      soldierView, enemyView, result, fieldView, 70)
    movement.start()
    assert(soldierView.x === 0)
    assert(soldierView.y === 10)
    assert(soldierView.state === SoldierViewAttackState(true, NE, 0))
    assert(soldierView.index === 0)

    movement.update(5000 / 4)
    assert(soldierView.x === 300 / 4 * 7 / 10)
    assert(soldierView.y === 10 + 100 * 7 / 10)
    assert(soldierView.state === SoldierViewAttackState(true, NE, 0))
    assert(soldierView.index === 1)

    movement.update(5000 / 4)
    assert(soldierView.x === 300 / 2 * 7 / 10)
    assert(soldierView.y === 10 + 200 * 7 / 10)
    assert(soldierView.state === SoldierViewAttackState(true, NE, 0))
    assert(soldierView.index === 2)

    movement.update(5000 / 4)
    assert(soldierView.x === 300 * 3 / 4 * 7 / 10)
    assert(soldierView.y === 10 + 300 * 7 / 10 - 1) // -1 is correction
    assert(soldierView.state === SoldierViewAttackState(true, NE, 0))
    assert(soldierView.index === 3)

    movement.update(5000 / 4)
    assert(soldierView.x === 300 * 7 / 10)
    assert(soldierView.y === 10 + 400 * 7 / 10)
    assert(soldierView.state === SoldierViewAttackState(true, NE, 0))
    assert(soldierView.index === 3)

    movement.update(5000 / 4)
    assert(soldierView.x === 300 * 3 / 4 * 7 / 10)
    assert(soldierView.y === 10 + 300 * 7 / 10)
    assert(soldierView.state === SoldierViewAttackState(true, NE, 0))
    assert(soldierView.index === 3)

    movement.update(5000 / 4)
    assert(soldierView.x === 300 / 2 * 7 / 10)
    assert(soldierView.y === 10 + 200 * 7 / 10)
    assert(soldierView.state === SoldierViewAttackState(true, NE, 0))
    assert(soldierView.index === 3)

    movement.update(5000 / 4)
    assert(soldierView.x === 300 / 4 * 7 / 10)
    assert(soldierView.y === 10 + 100 * 7 / 10)
    assert(soldierView.state === SoldierViewAttackState(true, NE, 0))
    assert(soldierView.index === 3)

    movement.update(5000 / 4)
    assert(soldierView.x === 0)
    assert(soldierView.y === 10)
    assert(soldierView.state === DefenceState)
    assert(soldierView.index === 0)

    assert(movement.isOver)

  }
}