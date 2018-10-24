package mr.merc.unit

import org.scalatest.FunSuite

object Util extends FunSuite {
  def soldierType(hp: Int, defence: Int, attackDamage: Int, attackCount: Int, attributes: Set[AttackAttribute] = Set(), resistance: Int = 0) =
    new SoldierType("type0", 1, hp, 1, 10, 1,
      List(new Attack(1, attackDamage, attackCount, Impact, false, attributes)),
      Map(), Map(SandDefence -> defence), Map(Impact -> resistance))
  def soldierType(defence: Int, attacks: List[Attack]) =
    new SoldierType("type0", 1, 100, 1, 10, 1,
      attacks, Map(), Map(SandDefence -> defence), Map(Impact -> 0))

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
}