package mr.merc.unit

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.terrain.{DesertSand, SandKind}
import mr.merc.battle.BattleModel
import mr.merc.map.hex.TerrainHex
import mr.merc.map.GameField
import mr.merc.players.Player

class CuresAttributeTest extends FunSuite with BeforeAndAfter {
  var field: TerrainHexField = _
  def simpleSoldierType(attributes: Set[SoldierTypeAttribute] = Set()) = new SoldierType("1", 1, 20, 10, 5, 1,
    List(Attack(0, 10, 1, Impact, false), Attack(1, 6, 2, Impact, false)), Map(SandKind -> 2),
    Map(SandDefence -> 60), Map(Impact -> 0), attributes, viewName = "")

  before {
    field = new TerrainHexField(10, 10, (x, y) => new TerrainHex(x, y, DesertSand))
  }

  test("cures poisoned neighbours before turn") {
    val poisonedSoldier1 = new Soldier("1", simpleSoldierType(), Player("1"))
    poisonedSoldier1.addState(Poisoned)
    val poisonedSoldier2 = new Soldier("2", simpleSoldierType(), Player("1"))
    poisonedSoldier2.addState(Poisoned)
    val curer = new Soldier("3", simpleSoldierType(Set(Cures)), Player("1"))
    field.hex(0, 1).soldier = Some(poisonedSoldier1)
    field.hex(1, 0).soldier = Some(curer)
    field.hex(5, 5).soldier = Some(poisonedSoldier2)
    val list = curer.beforeTurnActions(field, 1, 0)
    assert(list.size === 1)
    list(0) match {
      case CureSoldier(currentCurer, soldier) => {
        assert(soldier === poisonedSoldier1)
        assert(currentCurer === curer)
      }
      case _ => fail
    }
  }

  test("enemy is not cured") {
    val poisonedSoldier1 = new Soldier("1", simpleSoldierType(), Player("2"))
    poisonedSoldier1.addState(Poisoned)
    val poisonedSoldier2 = new Soldier("2", simpleSoldierType(), Player("1"))
    poisonedSoldier2.addState(Poisoned)
    val curer = new Soldier("3", simpleSoldierType(Set(Cures)), Player("1"))
    field.hex(0, 1).soldier = Some(poisonedSoldier1)
    field.hex(1, 0).soldier = Some(curer)
    field.hex(5, 5).soldier = Some(poisonedSoldier2)
    val list = curer.beforeTurnActions(field, 1, 0)
    assert(list.size === 0)
  }

  test("curing action is not called if ther is no need") {
    val poisonedSoldier1 = new Soldier("1", simpleSoldierType(), Player("1"))
    val curer = new Soldier("3", simpleSoldierType(Set(Cures)), Player("1"))
    field.hex(0, 1).soldier = Some(poisonedSoldier1)
    field.hex(1, 0).soldier = Some(curer)
    val list = curer.beforeTurnActions(field, 1, 0)
    assert(list.size === 0)
  }

  test("curing action works correctly") {
    val poisonedSoldier = new Soldier("1", simpleSoldierType(), Player("2"))
    poisonedSoldier.addState(Poisoned)
    CureSoldier(null, poisonedSoldier).action()
    assert(poisonedSoldier.state === Set())
  }
}