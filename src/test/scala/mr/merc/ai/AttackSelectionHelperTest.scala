package mr.merc.ai

import org.scalatest.FunSuite
import mr.merc.unit.Util._
import mr.merc.unit.Soldier
import mr.merc.players.Player
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import mr.merc.unit.Attack
import mr.merc.unit.Impact
import mr.merc.unit.Magical

class AttackSelectionHelperTest extends FunSuite {
  test("single attack is selected") {
    val attackerType = soldierType(100, 40, 5, 2, Set())

    val attacker = new Soldier("1", attackerType, Player("1"))
    val defender = new Soldier("2", attackerType, Player("2"))
    val attackerHex = new TerrainHex(0, 0, Sand)
    val defenderHex = new TerrainHex(0, 1, Sand)

    val attack = AttackSelectionHelper.selectBestAttack(attacker, defender, attackerHex, defenderHex)
    assert(attack === 0)
  }

  test("attack with bigger math expectation is selected by default") {
    val attackerType = soldierType(40, List(new Attack("", 10, 2, Impact,
      false), new Attack("", 5, 5, Impact, false)))

    val attacker = new Soldier("1", attackerType, Player("1"))
    val defender = new Soldier("2", attackerType, Player("2"))
    val attackerHex = new TerrainHex(0, 0, Sand)
    val defenderHex = new TerrainHex(0, 1, Sand)

    val attack = AttackSelectionHelper.selectBestAttack(attacker, defender, attackerHex, defenderHex)
    assert(attack === 1)
  }

  // magical attribute has different chance
  test("chance is taken into account when calculating math expectation") {
    val attackerType = soldierType(80, List(new Attack("", 10, 2, Impact, false,
      Set(Magical)), new Attack("", 5, 5, Impact, false)))

    val attacker = new Soldier("1", attackerType, Player("1"))
    val defender = new Soldier("2", attackerType, Player("2"))
    val attackerHex = new TerrainHex(0, 0, Sand)
    val defenderHex = new TerrainHex(0, 1, Sand)

    val attack = AttackSelectionHelper.selectBestAttack(attacker, defender, attackerHex, defenderHex)
    assert(attack === 0)
  }

  test("should select ranged/melee attack when defender doesn't have this attack") {
    val attackerType = soldierType(80, List(new Attack("", 10, 2, Impact, false,
      Set(Magical)), new Attack("", 1, 1, Impact, true)))
    val defenderType = soldierType(80, List(new Attack("", 10, 2, Impact, false)))
    val attacker = new Soldier("1", attackerType, Player("1"))
    val defender = new Soldier("2", defenderType, Player("2"))
    val attackerHex = new TerrainHex(0, 0, Sand)
    val defenderHex = new TerrainHex(0, 1, Sand)

    val attack = AttackSelectionHelper.selectBestAttack(attacker, defender, attackerHex, defenderHex)
    assert(attack === 1)
  }

  ignore("should use most effective attack when chances to kill enemy > 50%") {
    val attackerType = soldierType(80, List(new Attack("", 10, 2, Impact, false,
      Set(Magical)), new Attack("", 1, 1, Impact, true)))
    val defenderType = soldierType(80, List(new Attack("", 10, 2, Impact, false)))
    val attacker = new Soldier("1", attackerType, Player("1"))
    val defender = new Soldier("2", defenderType, Player("2"))
    val attackerHex = new TerrainHex(0, 0, Sand)
    val defenderHex = new TerrainHex(0, 1, Sand)
    defender.hp = 10

    val attack = AttackSelectionHelper.selectBestAttack(attacker, defender, attackerHex, defenderHex)
    assert(attack === 0)
  }
}