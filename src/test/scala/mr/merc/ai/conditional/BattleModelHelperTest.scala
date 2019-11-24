package mr.merc.ai.conditional

import org.scalatest.FunSuite
import mr.merc.players.Player
import mr.merc.unit.SoldierType
import mr.merc.unit.Soldier
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.GameField
import mr.merc.battle.BattleModel
import mr.merc.battle.event.EndMoveModelEvent

class BattleModelHelperTest extends FunSuite {
  private val player1 = Player("1")
  private val player2 = Player("2")
  private val player3 = Player("3")

  val sides = Set(Set(player1, player2), Set(player3))

  val soldierType = SoldierType("testSoldier")
  val soldier1 = new Soldier("1", soldierType, player1)
  val soldier2 = new Soldier("2", soldierType, player2)
  val soldier3 = new Soldier("3", soldierType, player3)
  val soldier4 = new Soldier("4", soldierType, player3)

  val map = new TerrainHexField(15, 15, TerrainHex.grassInit)
  val game = new GameField(map, List(player3, player1, player2), sides)
  map.hex(0, 0).soldier = Some(soldier1)
  map.hex(10, 2).soldier = Some(soldier2)
  map.hex(10, 0).soldier = Some(soldier3)
  map.hex(8, 13).soldier = Some(soldier4)
  val model = new BattleModel(game)

  import BattleModelHelper._

  test("currentSoldiers") {
    assert(model.currentSoldiers.toSet === Set((soldier3, map.hex(10, 0)), (soldier4, map.hex(8, 13))))
    model.handleEvent(EndMoveModelEvent)
    assert(model.currentSoldiers.toSet === Set((soldier1, map.hex(0, 0))))
    model.handleEvent(EndMoveModelEvent)
    model.handleEvent(EndMoveModelEvent)
  }

  test("enemies") {
    assert(model.enemies.toSet === Set((soldier1, map.hex(0, 0)), (soldier2, map.hex(10, 2))))
  }

  test("friends") {
    assert(model.friends.toSet === Set((soldier3, map.hex(10, 0)), (soldier4, map.hex(8, 13))))
    model.handleEvent(EndMoveModelEvent)
    assert(model.friends.toSet === Set((soldier1, map.hex(0, 0)), (soldier2, map.hex(10, 2))))
    model.handleEvent(EndMoveModelEvent)
    assert(model.friends.toSet === Set((soldier1, map.hex(0, 0)), (soldier2, map.hex(10, 2))))
    model.handleEvent(EndMoveModelEvent)
  }

  test("movedCurrentSoldiers") {
    assert(model.movedCurrentSoldiers === List())
    soldier3.movePointsRemain -= 1
    assert(model.movedCurrentSoldiers === List(soldier3))
    soldier3.movePointsRemain += 1
  }

  test("reachableEnemies") {
    assert(model.reachableEnemies(map.hex(10, 0), soldier3) === Set(map.hex(10, 2)))
    assert(model.reachableEnemies(map.hex(8, 13), soldier4) === Set())

  }

}