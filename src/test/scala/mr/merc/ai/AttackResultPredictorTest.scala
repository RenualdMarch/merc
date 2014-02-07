package mr.merc.ai

import org.scalatest.FunSuite
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain.Sand
import mr.merc.unit.Soldier
import mr.merc.players.Player
import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import mr.merc.unit.Firststrike
import mr.merc.unit.Berserk
import mr.merc.unit.Drain

class AttackResultPredictorTest extends FunSuite with Matchers {

  import mr.merc.unit.Util._

  val attackerHex = new TerrainHex(0, 0, Sand)
  val defenderHex = new TerrainHex(0, 1, Sand)

  test("simple prediction") {
    val attacker = new Soldier("1", soldierType(10, 60, 10, 1), Player("1"))
    val defender = new Soldier("2", soldierType(10, 40, 10, 1), Player("2"))

    val pred = AttackResultPredictor.predictResult(attacker, defender, attackerHex, defenderHex, attacker.soldierType.attacks(0), Some(defender.soldierType.attacks(0)))
    assert(pred.defenderDies === 0.6)
    assert(pred.attackerDies === 0.4 * 0.4)
    assert(pred.expectedDamageToAttacker === 0.4 * 0.4 * 10)
    assert(pred.expectedDamageToDefender === 0.6 * 10)
  }

  test("complex prediction") {
    val attacker = new Soldier("1", soldierType(10, 60, 5, 2), Player("1"))
    val defender = new Soldier("2", soldierType(10, 40, 5, 2), Player("2"))

    val pred = AttackResultPredictor.predictResult(attacker, defender, attackerHex, defenderHex, attacker.soldierType.attacks(0), Some(defender.soldierType.attacks(0)))
    assert(pred.defenderDies === 0.6 * 0.6)
    assert(pred.attackerDies === 0.4 * 0.4 * (1 - 0.6 * 0.6))
    pred.expectedDamageToAttacker should be(((1 - 0.6 * 0.6) * 10 * 0.4 + 0.6 * 0.6 * 5 * 0.4) +- 0.01)
    pred.expectedDamageToDefender should be(6d +- 0.01)
  }

  test("prediction with defender without attack") {
    val attacker = new Soldier("1", soldierType(10, 40, 5, 2), Player("1"))
    val defender = new Soldier("2", soldierType(10, 60, 10, 1), Player("2"))
    val pred = AttackResultPredictor.predictResult(attacker, defender, attackerHex, defenderHex, attacker.soldierType.attacks(0), None)
    assert(pred.attackerDies === 0)
    assert(pred.defenderDies === 0.4 * 0.4)
    assert(pred.expectedDamageToAttacker === 0)
    assert(pred.expectedDamageToDefender === 4)
  }

  test("prediction with defender first strike") {
    val attacker = new Soldier("1", soldierType(10, 40, 10, 1), Player("1"))
    val defender = new Soldier("2", soldierType(10, 60, 10, 1, Set(Firststrike)), Player("2"))

    val pred = AttackResultPredictor.predictResult(attacker, defender, attackerHex, defenderHex, attacker.soldierType.attacks(0), Some(defender.soldierType.attacks(0)))
    assert(pred.attackerDies === 0.6)
    assert(pred.defenderDies === 0.4 * 0.4)
    assert(pred.expectedDamageToDefender === 0.4 * 0.4 * 10)
    assert(pred.expectedDamageToAttacker === 0.6 * 10)
  }

  test("prediction with attacker first strike") {
    val attacker = new Soldier("1", soldierType(10, 60, 10, 1, Set(Firststrike)), Player("1"))
    val defender = new Soldier("2", soldierType(10, 40, 10, 1), Player("2"))

    val pred = AttackResultPredictor.predictResult(attacker, defender, attackerHex, defenderHex, attacker.soldierType.attacks(0), Some(defender.soldierType.attacks(0)))
    assert(pred.defenderDies === 0.6)
    assert(pred.attackerDies === 0.4 * 0.4)
    assert(pred.expectedDamageToAttacker === 0.4 * 0.4 * 10)
    assert(pred.expectedDamageToDefender === 0.6 * 10)
  }

  test("prediction with attacker berserk") {
    val attacker = new Soldier("1", soldierType(10, 60, 10, 1, Set(Berserk)), Player("1"))
    val defender = new Soldier("2", soldierType(10, 40, 0, 1), Player("2"))
    val pred = AttackResultPredictor.predictResult(attacker, defender, attackerHex, defenderHex, attacker.soldierType.attacks(0), Some(defender.soldierType.attacks(0)))
    assert(pred.defenderDies > 0.9)
    assert(pred.attackerDies === 0)
    assert(pred.expectedDamageToAttacker === 0)
    assert(pred.expectedDamageToDefender > 9)
  }

  test("prediction with defender berserk") {
    val attacker = new Soldier("1", soldierType(10, 60, 10, 1), Player("1"))
    val defender = new Soldier("2", soldierType(10, 40, 0, 1, Set(Berserk)), Player("2"))
    val pred = AttackResultPredictor.predictResult(attacker, defender, attackerHex, defenderHex, attacker.soldierType.attacks(0), Some(defender.soldierType.attacks(0)))
    assert(pred.defenderDies > 0.9)
    assert(pred.attackerDies === 0)
    assert(pred.expectedDamageToAttacker === 0)
    assert(pred.expectedDamageToDefender > 9)
  }

  test("prediction with attacker who drains") {
    val attacker = new Soldier("1", soldierType(10, 50, 10, 1, Set(Drain)), Player("1"))
    val defender = new Soldier("2", soldierType(10, 50, 10, 1), Player("2"))
    attacker.hp = 5
    val pred = AttackResultPredictor.predictResult(attacker, defender, attackerHex, defenderHex, attacker.soldierType.attacks(0), Some(defender.soldierType.attacks(0)))

    assert(pred.defenderDies === 0.5)
    assert(pred.attackerDies === 0.5 * 0.5)
    assert(pred.expectedDamageToAttacker === 0.5 * 0.5 * 5 - 0.5 * 5)
    assert(pred.expectedDamageToDefender === 10 / 2)
  }

  test("prediction with defender who drains") {
    val attacker = new Soldier("1", soldierType(10, 50, 5, 1), Player("1"))
    val defender = new Soldier("2", soldierType(10, 50, 10, 1, Set(Drain)), Player("2"))
    val pred = AttackResultPredictor.predictResult(attacker, defender, attackerHex, defenderHex, attacker.soldierType.attacks(0), Some(defender.soldierType.attacks(0)))
    assert(pred.defenderDies === 0)
    assert(pred.attackerDies === 0.5)
    assert(pred.expectedDamageToAttacker === 5)
    assert(pred.expectedDamageToDefender === (0.5 * 5) - (0.5 * 0.5 * 10 / 2))

  }
}