package mr.merc.unit

import org.scalatest.FunSuite
import mr.merc.players.Player
import mr.merc.map.terrain.TerrainType._
import mr.merc.map.hex.TerrainHex
import mr.merc.unit.SoldierState._
import mr.merc.unit.AttackAttribute._


class SlowedAttributeTest extends FunSuite {
  import TestUtil._
  val attackerType = soldierType(100, 50, 10, 1, Set(Slow))
  val defenderType = soldierType(100, 90, 10, 1, Set())
  val attackerHex = new TerrainHex(0, 0, DesertSand)
  val defenderHex = new TerrainHex(1, 0, DesertSand)
  test("defender is slowed when attack is successfull") {
    val attacker = new Soldier("1", attackerType, Player("1"))
	val defender = new Soldier("2", defenderType, Player("2"))
	attackerHex.soldier = Some(attacker)
	defenderHex.soldier = Some(defender)
	val result = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), None, i => true)
	assert(result.size === 1)
	assert(result(0).success === true)
	assert(defender.state === Set(Slowed))
  }
  
  test("defender is not slowed when attack is not successfull") {
    val attacker = new Soldier("1", attackerType, Player("1"))
	val defender = new Soldier("2", defenderType, Player("2"))
	attackerHex.soldier = Some(attacker)
	defenderHex.soldier = Some(defender)
	val result = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), None, i => false)
	assert(result.size === 1)
	assert(result(0).success === false)
	assert(defender.state === Set())
  }
}