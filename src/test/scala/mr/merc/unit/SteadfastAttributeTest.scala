package mr.merc.unit

import org.scalatest.FunSuite
import mr.merc.players.Player
import mr.merc.unit.AttackType._
import mr.merc.map.terrain.TerrainKind._
import mr.merc.unit.DefenceType._
import mr.merc.unit.SoldierTypeAttribute._

class SteadfastAttributeTest extends FunSuite {
  def simpleSoldierType(attributes: Set[SoldierTypeAttribute] = Set(), resistance: Int) = new SoldierType("1", 1, 20, 10, 5, 1,
    List(Attack(1, 10, 1, Impact, false), Attack(2, 6, 2, Impact, false)), Map(SandKind -> 2),
    Map(SandDefence -> 60), Map(Impact -> resistance), attributes, viewName = "")

  val steadfast = new Soldier("1", simpleSoldierType(Set(Steadfast), 10), Player("1"))

  test("resistance is double when defending") {
    val steadfast = new Soldier("1", simpleSoldierType(Set(Steadfast), 10), Player("1"))
    val usual = new Soldier("1", simpleSoldierType(Set(), 10), Player("1"))
    val damage = Attack.possibleAttackersDamage(true, usual, steadfast, usual.soldierType.attacks(0), None)
    assert(damage === 8)
  }

  test("resistance is not doubled when attacking") {
    val steadfast = new Soldier("1", simpleSoldierType(Set(Steadfast), 10), Player("1"))
    val usual = new Soldier("1", simpleSoldierType(Set(), 10), Player("1"))
    val damage = Attack.possibleAttackersDamage(false, usual, steadfast, usual.soldierType.attacks(0), None)
    assert(damage === 9)
  }

  test("resistance when doubled cann't be bigger than 50") {
    val steadfast = new Soldier("1", simpleSoldierType(Set(Steadfast), 40), Player("1"))
    val usual = new Soldier("1", simpleSoldierType(Set(), 10), Player("1"))
    val damage = Attack.possibleAttackersDamage(true, usual, steadfast, usual.soldierType.attacks(0), None)
    assert(damage === 5)
  }

  test("resistances smaller then 0 are not doubled") {
    val steadfast = new Soldier("1", simpleSoldierType(Set(Steadfast), -10), Player("1"))
    val usual = new Soldier("1", simpleSoldierType(Set(), 10), Player("1"))
    val damage = Attack.possibleAttackersDamage(true, usual, steadfast, usual.soldierType.attacks(0), None)
    assert(damage === 11)
  }

}