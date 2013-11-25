package mr.merc.unit

import org.scalatest.FunSuite
import mr.merc.players.Player
import mr.merc.map.terrain.Sand
import mr.merc.map.hex.TerrainHex
class MarksmanAttributeTest extends FunSuite {
  import Util._
  val attackerType = soldierType(100, 50, 10, 1, Set(Marksman))
  val strongDefenderType = soldierType(100, 90, 10, 1, Set())
  val weakDefenderType = soldierType(100, 30, 10, 1, Set())
  val attackerHex = new TerrainHex(0, 0, Sand)
  val defenderHex = new TerrainHex(1, 0, Sand)    
  test("marksman makes attack chance be equal to defence if defence is smaller") {
    val attacker = new Soldier("1", attackerType, Player("1"))
	val defender = new Soldier("2", weakDefenderType, Player("2"))
	attackerHex.soldier = Some(attacker)
	defenderHex.soldier = Some(defender)
	val result = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), None, i => i == 30)
	assert(result.size === 1)
	assert(result(0).success === true)
  }
  
  test("marksman makes attack change be 60 if defence is bigger") {
    val attacker = new Soldier("1", attackerType, Player("1"))
	val defender = new Soldier("2", strongDefenderType, Player("2"))
	attackerHex.soldier = Some(attacker)
	defenderHex.soldier = Some(defender)
	val result = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), None, i => i == 40)
	assert(result.size === 1)
	assert(result(0).success === true)
  }
}