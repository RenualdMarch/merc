package mr.merc.unit

import mr.merc.map.terrain.TerrainKind._
import org.scalatest.FunSuite
import AttackType._
import mr.merc.unit.DefenceType._
import mr.merc.unit.AttackAttribute._

object TestUtil extends FunSuite {
  def soldierType(hp: Int, defence: Int, attackDamage: Int, attackCount: Int, attributes: Set[AttackAttribute] = Set(), resistance: Int = 0) =
    new SoldierType("type0", 1, hp, 1, 10, 1,
      List(new Attack(1, attackDamage, attackCount, Impact, false, attributes)),
      Map(), Map(SandDefence -> defence), Map(Impact -> resistance), viewName = "")

  def soldierType(defence: Int, attacks: List[Attack]) =
    new SoldierType("type0", 1, 100, 1, 10, 1,
      attacks, Map(), Map(SandDefence -> defence), Map(Impact -> 0), viewName = "")

  def f(t: Int)(c: ChanceOfSuccess): Boolean = t <= c.chanceNumber

  def attackByAttackerGeneric(results: List[AttackResult], attacker: Soldier, defender: Soldier,
                              attackerType: SoldierType, defenderType: SoldierType,
                              isAttackerAttackingThisRound: Boolean, success: Boolean)(i: Int) = {
    val result = results(i)
    assert(result.attacker === attacker)
    assert(result.defender === defender)
    assert(result.attackersAttack === attackerType.attacks(0))
    assert(result.isAttackerAttackingThisRound === isAttackerAttackingThisRound)
    assert(result.success === success)
  }

  def attackByDefenderGeneric(results: List[AttackResult], attacker: Soldier, defender: Soldier,
                              attackerType: SoldierType, defenderType: SoldierType,
                              isAttackerAttackingThisRound: Boolean, success: Boolean)(i: Int) = {
    val result = results(i)
    assert(result.attacker === defender)
    assert(result.defender === attacker)
    assert(result.attackersAttack === defenderType.attacks(0))
    assert(result.isAttackerAttackingThisRound === isAttackerAttackingThisRound)
    assert(result.success === success)
  }

  def testSoldierType: SoldierType = SoldierType("testSoldier", 2, 3, 10, 45, 3,
    attacks = List(Attack(0, 3, 4, Impact, false), Attack(1, 2, 3, Fire, true, Set(Drain, Firststrike))),
    moveCost = Map(SandKind -> 1, GrassKind -> 2), defence = Map(SandDefence -> 50, GrassDefence -> 30),
    resistance = Map(Impact -> 0, Pierce -> 20, Arcane -> -30), viewName = "testSoldier")

  def testSoldierType2: SoldierType = testSoldierType.copy(name = "testSoldier2", viewName = "testSoldier2")

}