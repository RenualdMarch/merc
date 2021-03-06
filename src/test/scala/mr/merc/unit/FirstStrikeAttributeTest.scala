package mr.merc.unit

import org.scalatest.FunSuite
import mr.merc.players.Player
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain.TerrainType._
import mr.merc.unit.AttackAttribute._

class FirststrikeAttributeTest extends FunSuite {
  import TestUtil._
  val firstStrikeType = soldierType(100, 50, 10, 2, Set(Firststrike))
  val otherType = soldierType(100, 50, 10, 2, Set.empty)
  val attackerHex = new TerrainHex(0, 0, DesertSand)
  val defenderHex = new TerrainHex(1, 0, DesertSand)
  test("soldier with first strike attacks first when he is attacker") {

	val attacker = new Soldier("1", firstStrikeType, Player("1"))
	val defender = new Soldier("2", otherType, Player("2"))
	attackerHex.soldier = Some(attacker)
	defenderHex.soldier = Some(defender)
	
	val result = Attack.battle(attackerHex, defenderHex, firstStrikeType.attacks(0), Some(otherType.attacks(0)), f(60))
	assert(result(0).isAttackerAttackingThisRound === true)
	assert(result(0).attacker === attacker)
	assert(result(1).isAttackerAttackingThisRound === false)
	assert(result(1).attacker === defender)
	assert(result(2).isAttackerAttackingThisRound === true)
	assert(result(2).attacker === attacker)
	assert(result(3).isAttackerAttackingThisRound === false)
	assert(result(3).attacker === defender)
  }
  
  test("soldier with first strike attacks first when he is defender") {
    val attacker = new Soldier("1", otherType, Player("1"))
	val defender = new Soldier("2", firstStrikeType, Player("2"))
	attackerHex.soldier = Some(attacker)
	defenderHex.soldier = Some(defender)
	
	val result = Attack.battle(attackerHex, defenderHex, otherType.attacks(0), Some(firstStrikeType.attacks(0)), f(60))

	assert(result(0).isAttackerAttackingThisRound === false)
	assert(result(0).attacker === defender)
	assert(result(1).isAttackerAttackingThisRound === true)
	assert(result(1).attacker === attacker)
	assert(result(2).isAttackerAttackingThisRound === false)
	assert(result(2).attacker === defender)
	assert(result(3).isAttackerAttackingThisRound === true)
	assert(result(3).attacker === attacker)
  }
  
  test("attacker attacks first when both have first strike attribute") {
    val attacker = new Soldier("1", firstStrikeType, Player("1"))
	val defender = new Soldier("2", firstStrikeType, Player("2"))
	attackerHex.soldier = Some(attacker)
	defenderHex.soldier = Some(defender)
	
	val result = Attack.battle(attackerHex, defenderHex, firstStrikeType.attacks(0), Some(firstStrikeType.attacks(0)), f(60))
	assert(result(0).isAttackerAttackingThisRound === true)
	assert(result(0).attacker === attacker)
	assert(result(1).isAttackerAttackingThisRound === false)
	assert(result(1).attacker === defender)
	assert(result(2).isAttackerAttackingThisRound === true)
	assert(result(2).attacker === attacker)
	assert(result(3).isAttackerAttackingThisRound === false)
	assert(result(3).attacker === defender)
  }
}