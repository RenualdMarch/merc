package mr.merc.unit

import org.scalatest.FunSuite
import mr.merc.players.Player

class PoisonAttributeTest extends FunSuite {
  import Util._
  val attackerType = soldierType(100, 50, 10, 1, Set(Poison))
  val defenderType = soldierType(100, 90, 10, 1, Set())
  
  test("defender is poisoned when attack is successfull") {
    val attacker = new Soldier("1", attackerType, Player("1"))
	val defender = new Soldier("2", defenderType, Player("2"))
	attackerHex.soldier = Some(attacker)
	defenderHex.soldier = Some(defender)
	val result = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), None, i => true)
	assert(result.size === 1)
	assert(result(0).success === true)
	assert(defender.state === Set(Poisoned))
  }
  
  test("defender is not poisoned when attack is not successfull") {
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