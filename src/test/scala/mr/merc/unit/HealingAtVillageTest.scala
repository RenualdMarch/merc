package mr.merc.unit

import mr.merc.economics.Culture.LatinHuman
import org.scalatest.FunSuite
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.terrain.{DesertSand, SandKind, WallsKind}
import org.scalatest.BeforeAndAfter
import mr.merc.map.hex.TerrainHex
import mr.merc.map.objects.{House, SummerHouse}
import mr.merc.players.Player

class HealingAtVillageTest extends FunSuite with BeforeAndAfter {
  var field: TerrainHexField = _
  def simpleSoldierType(attributes: Set[SoldierTypeAttribute] = Set()) = new SoldierType("1", 1, 20, 10, 5, 1,
    List(), Map(SandKind -> 2, WallsKind -> 1),
    Map(SandDefence -> 60, BuildingDefence -> 60), Map(Impact -> 0), attributes, viewName = "")

  before {
    field = new TerrainHexField(10, 10, (x, y) => if (x == 5 && y == 5)
      new TerrainHex(x, y, DesertSand, Some(SummerHouse(LatinHuman)))
    else new TerrainHex(x, y, DesertSand))
  }

  test("Unit receives healing when he moved and is in village") {
    val soldier = new Soldier("1", simpleSoldierType(), Player("1"))
    soldier.movePointsRemain -= 1
    soldier.hp = 5
    field.hex(5, 5).soldier = Some(soldier)
    val list = soldier.beforeTurnActions(field, 5, 5)
    assert(list.size === 1)
    list(0) match {
      case Regeneration(regeneratedSoldier) => assert(regeneratedSoldier == soldier)
      case _ => fail
    }
    soldier.beforeTurnActions(field, 5, 5).foreach(_.action())
    assert(soldier.hp === 13)
  }

  test("Unit received bonus healing when he didn't moved") {
    val soldier = new Soldier("1", simpleSoldierType(), Player("1"))
    soldier.hp = 5
    field.hex(5, 5).soldier = Some(soldier)
    val list = soldier.beforeTurnActions(field, 5, 5)
    assert(list.size === 1)
    list(0) match {
      case Regeneration(regeneratedSoldier) => assert(regeneratedSoldier == soldier)
      case _ => fail
    }
    soldier.beforeTurnActions(field, 5, 5).foreach(_.action())
    assert(soldier.hp === 15)
  }

  test("Unit with regeneration doesn't receive two regeneration events") {
    val soldier = new Soldier("1", simpleSoldierType(Set(Regenerates)), Player("1"))
    soldier.hp = 5
    field.hex(5, 5).soldier = Some(soldier)
    val list = soldier.beforeTurnActions(field, 5, 5)
    assert(list.size === 1)
    list(0) match {
      case Regeneration(regeneratedSoldier) => assert(regeneratedSoldier == soldier)
      case _ => fail
    }
  }

  test("Unit with full health doesn't receive regeneration event") {
    val soldier = new Soldier("1", simpleSoldierType(Set(Regenerates)), Player("1"))
    field.hex(5, 5).soldier = Some(soldier)
    val list = soldier.beforeTurnActions(field, 5, 5)
    assert(list.size === 0)
  }

  test("Poisoned unit who regenerates receviced regeneration event") {
    val soldier = new Soldier("1", simpleSoldierType(Set(Regenerates)), Player("1"))
    soldier.addState(Poisoned)
    field.hex(5, 5).soldier = Some(soldier)
    val list = soldier.beforeTurnActions(field, 5, 5)
    assert(list.size === 1)
    list(0) match {
      case Regeneration(regeneratedSoldier) => assert(regeneratedSoldier == soldier)
      case _ => fail
    }
  }

  test("Poisoned unit receviced regeneration event") {
    val soldier = new Soldier("1", simpleSoldierType(Set()), Player("1"))
    soldier.addState(Poisoned)
    field.hex(5, 5).soldier = Some(soldier)
    val list = soldier.beforeTurnActions(field, 5, 5)
    assert(list.size === 1)
    list(0) match {
      case Regeneration(regeneratedSoldier) => assert(regeneratedSoldier == soldier)
      case _ => fail
    }
  }

  test("Poisoned and not healthy unit who regenerates received regeneration event") {
    val soldier = new Soldier("1", simpleSoldierType(Set(Regenerates)), Player("1"))
    soldier.addState(Poisoned)
    soldier.hp = 5
    field.hex(5, 5).soldier = Some(soldier)
    val list = soldier.beforeTurnActions(field, 5, 5)
    assert(list.size === 1)
    list(0) match {
      case Regeneration(regeneratedSoldier) => assert(regeneratedSoldier == soldier)
      case _ => fail
    }
  }

  test("Poisoned and not healthy unit received regeneration event") {
    val soldier = new Soldier("1", simpleSoldierType(Set()), Player("1"))
    soldier.addState(Poisoned)
    soldier.hp = 5
    field.hex(5, 5).soldier = Some(soldier)
    val list = soldier.beforeTurnActions(field, 5, 5)
    assert(list.size === 1)
    list(0) match {
      case Regeneration(regeneratedSoldier) => assert(regeneratedSoldier == soldier)
      case _ => fail
    }
  }
}