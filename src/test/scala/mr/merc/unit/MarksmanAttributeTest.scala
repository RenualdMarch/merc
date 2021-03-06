package mr.merc.unit

import org.scalatest.FunSuite
import mr.merc.players.Player
import mr.merc.map.terrain.TerrainType._
import mr.merc.map.hex.TerrainHex
import mr.merc.unit.AttackAttribute._

class MarksmanAttributeTest extends FunSuite {
  import TestUtil._
  val attackerType = soldierType(100, 50, 10, 1, Set(Marksman))
  val strongDefenderType = soldierType(100, 90, 10, 1, Set())
  val weakDefenderType = soldierType(100, 30, 10, 1, Set())
  val attackerHex = new TerrainHex(0, 0, DesertSand)
  val defenderHex = new TerrainHex(1, 0, DesertSand)
  test("marksman makes attack chance be equal to what it should have been if defence is smaller") {
    val attacker = new Soldier("1", attackerType, Player("1"))
    val defender = new Soldier("2", weakDefenderType, Player("2"))
    attackerHex.soldier = Some(attacker)
    defenderHex.soldier = Some(defender)
    val result = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), None, i => i.chanceNumber == 70)
    assert(result.size === 1)
    assert(result(0).success === true)
  }

  test("marksman makes attack change be 60 if defence is bigger") {
    val attacker = new Soldier("1", attackerType, Player("1"))
    val defender = new Soldier("2", strongDefenderType, Player("2"))
    attackerHex.soldier = Some(attacker)
    defenderHex.soldier = Some(defender)
    val result = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), None, i => i.chanceNumber == 60)
    assert(result.size === 1)
    assert(result(0).success === true)
  }
}