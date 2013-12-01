package mr.merc.unit

import org.scalatest.FunSuite
import mr.merc.players.Player
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain.Sand

class SoldierStateTest extends FunSuite{
    val soldierType = new SoldierType("type0", 1, 20, 1, 10, 1, 
		List(), Map(), Map(), Map())
  
	test("poisoned state results in 8 hp drop") {
	  val soldier = new Soldier("1", soldierType, Player("1"))
	  soldier.addState(Poisoned)
	  PoisoningDamage(soldier).action()
	  assert(soldier.hp === 12)
	}
	
	test("poisonined doesn't go below 1 hp") {
	  val soldier = new Soldier("1", soldierType, Player("1"))
	  soldier.addState(Poisoned)
	  soldier.hp = 8
	  PoisoningDamage(soldier).action()
	  assert(soldier.hp === 1)
	  soldier.hp = 5
	  PoisoningDamage(soldier).action()
	  assert(soldier.hp === 1)
	}
	
	test("slowed unit possible damage is halfed when") {
	  val someType = Util.soldierType(100, 50, 10, 3, Set())
	  val anotherType = Util.soldierType(40, 20, 3, 2, Set())
      val attackerHex = new TerrainHex(0, 0, Sand)
      val defenderHex = new TerrainHex(1, 0, Sand)
	  val attacker = new Soldier("1", someType, Player("1"))
	  attacker.addState(Slowed)
	  val defender = new Soldier("2", anotherType, Player("2"))
	  attackerHex.soldier = Some(attacker)
	  defenderHex.soldier = Some(defender)
	  val attackerDamage = Attack.possibleAttackersDamage(true, attacker, defender, someType.attacks(0), Some(anotherType.attacks(0)))
	  assert(attackerDamage === 5)
	  val attackerAsDefenderDamage = Attack.possibleAttackersDamage(false, attacker, defender, someType.attacks(0), Some(anotherType.attacks(0)))
	  assert(attackerAsDefenderDamage === 5)
	}
	
	test("slowing dissapears after new turn begins") {
	  val someType = Util.soldierType(100, 50, 10, 3, Set())
	  val soldier = new Soldier("1", someType, Player("1")) 
	  soldier.addState(Slowed)
	  assert(soldier.state === Set(Slowed))
	  soldier.beforeTurnRenowation()
	  assert(soldier.state === Set())
	}
	
	test("slowing is taken into account when filtering attacks") {
	  val attackerType = Util.soldierType(100, 50, 10, 5, Set())
	  val defenderType = Util.soldierType(25, 50, 5, 3, Set(Slow))
	  val attackerHex = new TerrainHex(0, 0, Sand)
      val defenderHex = new TerrainHex(1, 0, Sand)
	  val attacker = new Soldier("1", attackerType, Player("1"))
	  val defender = new Soldier("2", defenderType, Player("2"))
	  attackerHex.soldier = Some(attacker)
	  defenderHex.soldier = Some(defender)
	  val result = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), Some(defenderType.attacks(0)), i => true)
	  assert(result.size === 7)
	  assert(attacker.hp === 85)
	  assert(defender.hp === 0)
	}
	
	test("slowing is taken into account when filtering attacks if unit is already slowed") {
	  val attackerType = Util.soldierType(100, 50, 10, 5, Set())
	  val defenderType = Util.soldierType(30, 50, 5, 3, Set())
	  val attackerHex = new TerrainHex(0, 0, Sand)
      val defenderHex = new TerrainHex(1, 0, Sand)
	  val attacker = new Soldier("1", attackerType, Player("1"))
	  val defender = new Soldier("2", defenderType, Player("2"))
	  attacker.addState(Slowed)
	  attackerHex.soldier = Some(attacker)
	  defenderHex.soldier = Some(defender)
	  val result = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), Some(defenderType.attacks(0)), i => true)
	  assert(result.size === 8)
	  assert(attacker.hp === 85)
	  assert(defender.hp === 5)
	}
	
	test("unit become slowed after successful attack of unit with slowing attriubute") {
	  val attackerType = Util.soldierType(100, 50, 10, 1, Set())
	  val defenderType = Util.soldierType(30, 50, 5, 1, Set(Slow))
	  val attackerHex = new TerrainHex(0, 0, Sand)
      val defenderHex = new TerrainHex(1, 0, Sand)
	  val attacker = new Soldier("1", attackerType, Player("1"))
	  val defender = new Soldier("2", defenderType, Player("2"))
	  attackerHex.soldier = Some(attacker)
	  defenderHex.soldier = Some(defender)
	  val result = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), Some(defenderType.attacks(0)), i => true)
	  assert(attacker.state.contains(Slowed))
	}
}