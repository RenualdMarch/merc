package mr.merc.unit
import org.scalatest.FunSuite
import mr.merc.players.Player
import mr.merc.map.terrain.Sand
import mr.merc.map.hex.TerrainHex
class ChargeAttributeTest extends FunSuite {
  import Util._
  val attackerHex = new TerrainHex(0, 0, Sand)
  val defenderHex = new TerrainHex(1, 0, Sand)  
  
  test("charge works when offensive") {
      val attackerType = soldierType(100, 50, 5, 2, Set(Charge))
	  val defenderType = soldierType(100, 50, 7, 1, Set.empty)
	  val attacker = new Soldier("1", attackerType, Player("1"))
	  val defender = new Soldier("2", defenderType, Player("2"))
	  attackerHex.soldier = Some(attacker)
	  defenderHex.soldier = Some(defender)
	  val attackersDamage = Attack.possibleAttackersDamage(true, attacker, defender, attackerType.attacks(0), Some(defenderType.attacks(0)))
	  val defendersDamage = Attack.possibleAttackersDamage(false, defender, attacker, defenderType.attacks(0), Some(attackerType.attacks(0)))
	  assert(attackersDamage === 10)
	  assert(defendersDamage === 14)
  }
  
  test("charge doesn't work when defensive") {
      val attackerType = soldierType(100, 50, 5, 2, Set())
	  val defenderType = soldierType(100, 50, 7, 1, Set(Charge))
	  val attacker = new Soldier("1", attackerType, Player("1"))
	  val defender = new Soldier("2", defenderType, Player("2"))
	  attackerHex.soldier = Some(attacker)
	  defenderHex.soldier = Some(defender)
	  val attackersDamage = Attack.possibleAttackersDamage(true, attacker, defender, attackerType.attacks(0), Some(defenderType.attacks(0)))
	  val defendersDamage = Attack.possibleAttackersDamage(false, defender, attacker, defenderType.attacks(0), Some(attackerType.attacks(0)))
	  assert(attackersDamage === 5)
	  assert(defendersDamage === 7)
  }
  
  test("charge works with resistances") {
      val attackerType = soldierType(100, 50, 10, 2, Set(Charge), -20)
	  val defenderType = soldierType(100, 50, 10, 1, Set.empty, 20)
	  val attacker = new Soldier("1", attackerType, Player("1"))
	  val defender = new Soldier("2", defenderType, Player("2"))
	  attackerHex.soldier = Some(attacker)
	  defenderHex.soldier = Some(defender)
	  val attackersDamage = Attack.possibleAttackersDamage(true, attacker, defender, attackerType.attacks(0), Some(defenderType.attacks(0)))
	  val defendersDamage = Attack.possibleAttackersDamage(false, defender, attacker, defenderType.attacks(0), Some(attackerType.attacks(0)))
	  assert(attackersDamage === 16)
	  assert(defendersDamage === 24)
  }  
}