package mr.merc.unit
import org.scalatest.FunSuite
import mr.merc.players.Player
import org.scalatest.BeforeAndAfter
import mr.merc.map.terrain.TerrainType._
import mr.merc.map.hex.TerrainHex
import mr.merc.unit.AttackAttribute._

class BerserkAttributeTest extends FunSuite with BeforeAndAfter {
  import TestUtil._

  val attackerHex = new TerrainHex(0, 0, DesertSand)
  val defenderHex = new TerrainHex(1, 0, DesertSand)
  test("berserk attribute in attack") {
    val attackerType = soldierType(10, 70, 5, 2, Set(Berserk))
    val defenderType = soldierType(30, 50, 5, 2, Set.empty)
    val attacker = new Soldier("1", attackerType, Player("1"))
    val defender = new Soldier("2", defenderType, Player("2"))
    attackerHex.soldier = Some(attacker)
    defenderHex.soldier = Some(defender)
    val results = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), Some(defenderType.attacks(0)), f(40))
    assert(results.size === 11)

    val attackByAttacker = attackByAttackerGeneric(results, attacker, defender, attackerType, defenderType, true, true) _
    val attackByDefender = attackByDefenderGeneric(results, attacker, defender, attackerType, defenderType, false, false) _

    attackByAttacker(0)
    attackByDefender(1)
    attackByAttacker(2)
    attackByDefender(3)
    attackByAttacker(4)
    attackByDefender(5)
    attackByAttacker(6)
    attackByDefender(7)
    attackByAttacker(8)
    attackByDefender(9)
    attackByAttacker(10)
  }

  test("berserk attribute in defence") {
    val attackerType = soldierType(30, 50, 5, 2, Set.empty)
    val defenderType = soldierType(10, 70, 5, 2, Set(Berserk))
    val attacker = new Soldier("1", attackerType, Player("1"))
    val defender = new Soldier("2", defenderType, Player("2"))
    attackerHex.soldier = Some(attacker)
    defenderHex.soldier = Some(defender)
    val results = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), Some(defenderType.attacks(0)), f(40))
    assert(results.size === 12)

    val attackByAttacker = attackByAttackerGeneric(results, attacker, defender, attackerType, defenderType, true, false) _
    val attackByDefender = attackByDefenderGeneric(results, attacker, defender, attackerType, defenderType, false, true) _

    attackByAttacker(0)
    attackByDefender(1)
    attackByAttacker(2)
    attackByDefender(3)
    attackByAttacker(4)
    attackByDefender(5)
    attackByAttacker(6)
    attackByDefender(7)
    attackByAttacker(8)
    attackByDefender(9)
    attackByAttacker(10)
    attackByDefender(11)
  }

  test("berserk attribute lasts only for 30 rounds") {
    val attackerType = soldierType(10, 50, 5, 2, Set(Berserk))
    val defenderType = soldierType(30, 50, 5, 2, Set.empty)
    val attacker = new Soldier("1", attackerType, Player("1"))
    val defender = new Soldier("2", defenderType, Player("2"))
    attackerHex.soldier = Some(attacker)
    defenderHex.soldier = Some(defender)
    val results = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), Some(defenderType.attacks(0)), f(60))
    assert(results.size === 4 * 30)
  }
}