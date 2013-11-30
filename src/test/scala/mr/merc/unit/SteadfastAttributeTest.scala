package mr.merc.unit

import org.scalatest.FunSuite

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.terrain.Sand
import mr.merc.battle.BattleModel
import mr.merc.map.hex.TerrainHex
import mr.merc.map.GameField
import mr.merc.players.Player

class SteadfastAttributeTest extends FunSuite {
  def simpleSoldierType(attributes:Set[SoldierTypeAttribute] = Set(), resistance:Int) = new SoldierType("1", 1, 20, 10, 5, 1, 
			List(Attack("", 10, 1, Impact, false), Attack("", 6, 2, Impact, false)), Map(Sand -> 2), 
			Map(Sand -> 60), Map(Impact -> resistance), attributes)
  
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