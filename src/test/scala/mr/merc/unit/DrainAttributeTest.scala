package mr.merc.unit

import org.scalatest.FunSuite
import mr.merc.players.Player
import mr.merc.map.terrain.Sand
import mr.merc.map.hex.TerrainHex
class DrainAttributeTest extends FunSuite {
  import Util._
  val attackerHex = new TerrainHex(0, 0, Sand)
  val defenderHex = new TerrainHex(1, 0, Sand)
  test("unit which doesn't have drain ability has drain always 0") {
    val someType = soldierType(15, 50, 10, 2, Set(), 0)
    val attacker = new Soldier("1", someType, Player("1"))
    val defender = new Soldier("2", someType, Player("2"))
    attackerHex.soldier = Some(attacker)
    defenderHex.soldier = Some(defender)
    val result = Attack.battle(attackerHex, defenderHex, someType.attacks(0), Some(someType.attacks(0)), f(40))
    assert(result.size === 3)
    result foreach (r => assert(r.drained === 0))
  }

  test("unit which has drain ablity") {
    val attackerType = soldierType(100, 50, 10, 2, Set(Drain))
    val defenderType = soldierType(100, 50, 10, 1, Set(Drain))
    val attacker = new Soldier("1", attackerType, Player("1"))
    val defender = new Soldier("2", defenderType, Player("2"))
    attackerHex.soldier = Some(attacker)
    defenderHex.soldier = Some(defender)
    val result = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), Some(defenderType.attacks(0)), f(40))
    assert(result.size === 3)
    assert(result(0).drained === 0)
    assert(result(1).drained === 5)
    assert(result(2).drained === 5)
  }

  test("drained health is added during apply damage state") {
    val someType = soldierType(15, 50, 10, 2, Set(Drain), 0)
    val attacker = new Soldier("1", someType, Player("1"))
    val defender = new Soldier("2", someType, Player("2"))
    attacker.hp = 7
    attackerHex.soldier = Some(attacker)
    defenderHex.soldier = Some(defender)
    val result = new AttackResult(true, attacker, defender, someType.attacks(0), Some(someType.attacks(0)), true, 10, 5)
    result.applyDamage()
    assert(attacker.hp === 12)
    assert(defender.hp === 5)
  }

  test("drained life is added immidiately after successful attack") {
    val attackerType = soldierType(15, 50, 20, 4, Set(Drain))
    val defenderType = soldierType(100, 50, 10, 4, Set.empty)
    val attacker = new Soldier("1", attackerType, Player("1"))
    val defender = new Soldier("2", defenderType, Player("2"))
    attackerHex.soldier = Some(attacker)
    defenderHex.soldier = Some(defender)
    val results = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), Some(defenderType.attacks(0)), f(40))
    assert(results.size === 8)
  }

  test("drained life is not added after failed attack") {
    val someType = soldierType(15, 50, 10, 2, Set(Drain), 0)
    val attacker = new Soldier("1", someType, Player("1"))
    val defender = new Soldier("2", someType, Player("2"))
    attacker.hp = 7
    attackerHex.soldier = Some(attacker)
    defenderHex.soldier = Some(defender)
    val result = new AttackResult(true, attacker, defender, someType.attacks(0), Some(someType.attacks(0)), false, 10, 5)
    result.applyDamage()
    assert(attacker.hp === 7)
    assert(defender.hp === 15)
  }

  test("drained life cann't make hp bigger than soldier type limits") {
    val someType = soldierType(15, 50, 10, 2, Set(Drain), 0)
    val attacker = new Soldier("1", someType, Player("1"))
    val defender = new Soldier("2", someType, Player("2"))
    attacker.hp = 14
    attackerHex.soldier = Some(attacker)
    defenderHex.soldier = Some(defender)
    val result = new AttackResult(true, attacker, defender, someType.attacks(0), Some(someType.attacks(0)), true, 10, 5)
    result.applyDamage()
    assert(attacker.hp === 15)
    assert(defender.hp === 5)
  }
}
