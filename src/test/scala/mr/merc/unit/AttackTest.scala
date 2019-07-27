package mr.merc.unit

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import mr.merc.map.hex.TerrainHex
import mr.merc.map.objects.House.HumanCityHouse
import mr.merc.players.Player
import mr.merc.map.terrain._
import mr.merc.map.objects._

class AttackTest extends FunSuite with BeforeAndAfter {
  val player1 = Player("1")
  val player2 = Player("2")
  val firstType = new SoldierType("type1", 1, 10, 1, 10, 1,
    List(new Attack(1, 3, 2, Impact, false), new Attack(2, 1, 2, Pierce, true)),
    Map(), Map(SandDefence -> 50), Map(Impact -> 0, Pierce -> 0), viewName = "")

  val secondType = new SoldierType("type2", 1, 12, 1, 10, 1,
    List(new Attack(1, 2, 3, Impact, false)),
    Map(), Map(SandDefence -> 30), Map(Impact -> 0, Pierce -> 0), viewName = "")

  var firstSoldier: Soldier = _
  var secondSoldier: Soldier = _
  val attackerHex = new TerrainHex(0, 0, DesertSand)
  val defenderHex = new TerrainHex(0, 1, DesertSand)

  def allMisses(x: ChanceOfSuccess) = false
  def firstHitsSecondMisses(x: ChanceOfSuccess) = if (x.chanceNumber == 70) true
  else if (x.chanceNumber == 50) false else fail

  before {
    firstSoldier = new Soldier("first", firstType, player1)
    secondSoldier = new Soldier("second", secondType, player2)
    attackerHex.soldier = Some(firstSoldier)
    defenderHex.soldier = Some(secondSoldier)
  }

  test("attack and defence both strike") {
    val results = Attack.battle(attackerHex, defenderHex, firstType.attacks(0), Some(secondType.attacks(0)), allMisses)
    assert(results.size === 5)
    assert(results(0).isAttackerAttackingThisRound === true)
    assert(results(0).attacker === firstSoldier)
    assert(results(0).defender === secondSoldier)
    assert(results(0).attackersAttack === firstType.attacks(0))
    assert(results(0).success === false)

    assert(results(1).isAttackerAttackingThisRound === false)
    assert(results(1).attacker === secondSoldier)
    assert(results(1).defender === firstSoldier)
    assert(results(1).attackersAttack === secondType.attacks(0))
    assert(results(1).success === false)

    assert(results(2).isAttackerAttackingThisRound === true)
    assert(results(2).attacker === firstSoldier)
    assert(results(2).defender === secondSoldier)
    assert(results(2).attackersAttack === firstType.attacks(0))
    assert(results(2).success === false)

    assert(results(3).isAttackerAttackingThisRound === false)
    assert(results(3).attacker === secondSoldier)
    assert(results(3).defender === firstSoldier)
    assert(results(3).attackersAttack === secondType.attacks(0))
    assert(results(3).success === false)

    assert(results(4).isAttackerAttackingThisRound === false)
    assert(results(4).attacker === secondSoldier)
    assert(results(4).defender === firstSoldier)
    assert(results(4).attackersAttack === secondType.attacks(0))
    assert(results(4).success === false)
  }

  test("only attack strikes") {
    val results = Attack.battle(attackerHex, defenderHex, firstType.attacks(1), None, allMisses)
    assert(results.size === 2)
    assert(results(0).isAttackerAttackingThisRound === true)
    assert(results(0).attacker === firstSoldier)
    assert(results(0).defender === secondSoldier)
    assert(results(0).attackersAttack === firstType.attacks(1))
    assert(results(0).success === false)

    assert(results(1).isAttackerAttackingThisRound === true)
    assert(results(1).attacker === firstSoldier)
    assert(results(1).defender === secondSoldier)
    assert(results(1).attackersAttack === firstType.attacks(1))
    assert(results(1).success === false)
  }

  test("attacks and death") {
    val firstResults = Attack.battle(attackerHex, defenderHex, firstType.attacks(0), Some(secondType.attacks(0)), firstHitsSecondMisses)

    assert(firstResults.size === 5)
    assert(firstResults(0).isAttackerAttackingThisRound === true)
    assert(firstResults(0).attacker === firstSoldier)
    assert(firstResults(0).defender === secondSoldier)
    assert(firstResults(0).attackersAttack === firstType.attacks(0))
    assert(firstResults(0).success === true)

    assert(firstResults(1).isAttackerAttackingThisRound === false)
    assert(firstResults(1).attacker === secondSoldier)
    assert(firstResults(1).defender === firstSoldier)
    assert(firstResults(1).attackersAttack === secondType.attacks(0))
    assert(firstResults(1).success === false)

    assert(firstResults(2).isAttackerAttackingThisRound === true)
    assert(firstResults(2).attacker === firstSoldier)
    assert(firstResults(2).defender === secondSoldier)
    assert(firstResults(2).attackersAttack === firstType.attacks(0))
    assert(firstResults(2).success === true)

    assert(firstResults(3).isAttackerAttackingThisRound === false)
    assert(firstResults(3).attacker === secondSoldier)
    assert(firstResults(3).defender === firstSoldier)
    assert(firstResults(3).attackersAttack === secondType.attacks(0))
    assert(firstResults(3).success === false)

    assert(firstResults(4).isAttackerAttackingThisRound === false)
    assert(firstResults(4).attacker === secondSoldier)
    assert(firstResults(4).defender === firstSoldier)
    assert(firstResults(4).attackersAttack === secondType.attacks(0))
    assert(firstResults(4).success === false)

    val secondResults = Attack.battle(attackerHex, defenderHex, firstType.attacks(0), Some(secondType.attacks(0)), firstHitsSecondMisses)

    assert(secondResults.size === 3)
    assert(secondResults(0).isAttackerAttackingThisRound === true)
    assert(secondResults(0).attacker === firstSoldier)
    assert(secondResults(0).defender === secondSoldier)
    assert(secondResults(0).attackersAttack === firstType.attacks(0))
    assert(secondResults(0).success === true)

    assert(secondResults(1).isAttackerAttackingThisRound === false)
    assert(secondResults(1).attacker === secondSoldier)
    assert(secondResults(1).defender === firstSoldier)
    assert(secondResults(1).attackersAttack === secondType.attacks(0))
    assert(secondResults(1).success === false)

    assert(secondResults(2).isAttackerAttackingThisRound === true)
    assert(secondResults(2).attacker === firstSoldier)
    assert(secondResults(2).defender === secondSoldier)
    assert(secondResults(2).attackersAttack === firstType.attacks(0))
    assert(secondResults(2).success === true)

    assert(secondSoldier.hp === 0)
    assert(firstSoldier.hp === 10)
  }

  test("village defence is separate thing") {
    val hex1 = new TerrainHex(0, 0, DesertSand)
    val hex2 = new TerrainHex(0, 0, DesertSand, Some(HumanCityHouse))

    val someType = new SoldierType("type1", 1, 10, 1, 10, 1,
      List(), Map(), Map(SandDefence -> 50, BuildingDefence -> 60),
      Map(Impact -> 0, Pierce -> 0), viewName = "")

    val soldier = new Soldier("1", someType, Player("1"))
    assert(Attack.calculateSoldierDefence(soldier, hex1).defence === 50)
    assert(Attack.calculateSoldierDefence(soldier, hex2).defence === 60)
  }

  test("wooden bridge and road defence same as grass") {
    val hex1 = new TerrainHex(0, 0, OldRoad)
    val hex2 = new TerrainHex(0, 0, ShallowWater, Some(WoodenBridge))

    val someType = new SoldierType("type1", 1, 10, 1, 10, 1,
      List(), Map(), Map(SandDefence -> 50, GrassDefence -> 60),
      Map(Impact -> 0, Pierce -> 0), viewName = "")

    val soldier = new Soldier("1", someType, Player("1"))
    assert(Attack.calculateSoldierDefence(soldier, hex1).defence === 60)
    assert(Attack.calculateSoldierDefence(soldier, hex2).defence === 60)
  }
}