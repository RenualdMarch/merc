package mr.merc.view.move

import mr.merc.map.hex._
import mr.merc.map.terrain.TerrainType._
import org.scalatest.FunSuite
import mr.merc.unit.{Soldier, SoldierType, TestUtil}
import mr.merc.unit.view.SoldierView
import mr.merc.map.hex.view.TerrainHexFieldView
import mr.merc.players.Player
import org.scalatest.BeforeAndAfter
import mr.merc.view.Sprite
import mr.merc.map.view.SoldiersDrawer

class SoldierMoveMovementTest extends FunSuite with BeforeAndAfter {
  val map = new TerrainHexField(5, 5, (x, y) => new TerrainHex(x, y, GreenGrass))
  val mapView = new TerrainHexFieldView(map, new SoldiersDrawer(), 1.0)
  val soldier = new Soldier("1", TestUtil.testSoldierType, Player("0"))
  map.hex(0, 1).soldier = Some(soldier)
  val soldierView = new SoldierView(soldier, 1.0)
  test("simple movement") {
    // moving from (0, 1) to (1, 0)
    val from = mapView.hex(0, 1)
    val to = mapView.hex(1, 0)
    val movement = new SoldierMoveMovement(from, to, soldierView, mapView)
    movement.start()
    assert(soldierView.x === 0)
    assert(soldierView.y === 72)
    while (!movement.isOver) {
      movement.update(1000)
    }

    assert(soldierView.x === 72 * 3 / 4)
    assert(soldierView.y === 36)
  }
}