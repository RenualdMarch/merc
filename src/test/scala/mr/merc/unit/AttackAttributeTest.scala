package mr.merc.unit

import org.scalatest.FunSuite
import mr.merc.map.terrain.Sand
import mr.merc.map.hex.TerrainHex
import mr.merc.players.Player
import org.scalatest.BeforeAndAfter
import org.scalatest.FunSuite
import org.scalatest.FunSuite

object Util extends FunSuite {
  
  	val attackerHex = new TerrainHex(0, 0, Sand)
	val defenderHex = new TerrainHex(1, 0, Sand)
	
  	def soldierType(hp:Int, defence:Int, attackDamage:Int, attackCount:Int, attributes:Set[AttackAttribute], resistance:Int = 0) = 
	  new SoldierType("type0", 1, hp, 1, 10, 1, 
		List(new Attack("name", attackDamage, attackCount, Impact, false, attributes)), 
		Map(), Map(Sand -> defence), Map(Impact -> resistance))
  	def f(t:Int)(d:Int):Boolean = t >= d
  	
  	def attackByAttackerGeneric(results:List[AttackResult], attacker:Soldier, defender:Soldier, 
  	      attackerType:SoldierType, defenderType:SoldierType,
  	      isAttackerAttackingThisRound:Boolean, success:Boolean)(i:Int) = {
	  val result = results(i)
	  assert(result.attacker === attacker)
	  assert(result.defender === defender)
	  assert(result.attackersAttack === attackerType.attacks(0))
	  assert(result.defendersAttack.get === defenderType.attacks(0))
	  assert(result.isAttackerAttackingThisRound === isAttackerAttackingThisRound)
	  assert(result.success === success)
	}
	  
	def attackByDefenderGeneric(results:List[AttackResult], attacker:Soldier, defender:Soldier, 
  	      attackerType:SoldierType, defenderType:SoldierType,
  	      isAttackerAttackingThisRound:Boolean, success:Boolean)(i:Int) = {
	  val result = results(i)
	  assert(result.attacker === defender)
	  assert(result.defender === attacker)
	  assert(result.attackersAttack === defenderType.attacks(0))
	  assert(result.defendersAttack.get === attackerType.attacks(0))
	  assert(result.isAttackerAttackingThisRound === isAttackerAttackingThisRound)
	  assert(result.success === success)
	}
}

class BerserkAttributeTest extends FunSuite with BeforeAndAfter {
	import Util._
  
    test("berserk attribute in attack") {
	  val attackerType = soldierType(10, 70, 5, 2, Set(Berserk))
	  val defenderType = soldierType(30, 50, 5, 2, Set.empty)
	  val attacker = new Soldier("1", attackerType, Player("1"))
	  val defender = new Soldier("2", defenderType, Player("2"))
	  attackerHex.soldier = Some(attacker)
	  defenderHex.soldier = Some(defender)
	  val results = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), Some(defenderType.attacks(0)), f(60))
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
	  val results = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), Some(defenderType.attacks(0)), f(60))
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
      val attackerType = soldierType(10, 70, 5, 2, Set(Berserk))
	  val defenderType = soldierType(30, 70, 5, 2, Set.empty)
	  val attacker = new Soldier("1", attackerType, Player("1"))
	  val defender = new Soldier("2", defenderType, Player("2"))
	  attackerHex.soldier = Some(attacker)
	  defenderHex.soldier = Some(defender)
	  val results = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), Some(defenderType.attacks(0)), f(60))
	  assert(results.size === 4 * 30)
    }
}

class ChargeAttributeTest extends FunSuite {
  import Util._
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
      val attackerType = soldierType(100, 50, 10, 2, Set(Charge), 20)
	  val defenderType = soldierType(100, 50, 10, 1, Set.empty, -20)
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

class DrainAttributeTest extends FunSuite {
  import Util._
  
  test("unit which doesn't have drain ability has drain always 0") {
      val someType = soldierType(15, 50, 10, 2, Set(), 0)
      val attacker = new Soldier("1", someType, Player("1"))
	  val defender = new Soldier("2", someType, Player("2"))
	  attackerHex.soldier = Some(attacker)
	  defenderHex.soldier = Some(defender)
	  val result = Attack.battle(attackerHex, defenderHex,someType.attacks(0), Some(someType.attacks(0)), f(60))
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
	  val result = Attack.battle(attackerHex, defenderHex,attackerType.attacks(0), Some(defenderType.attacks(0)), f(60))
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
    val results = Attack.battle(attackerHex, defenderHex, attackerType.attacks(0), Some(defenderType.attacks(0)), f(60))
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

class FirststrikeAttributeTest extends FunSuite {
  import Util._
  val firstStrikeType = soldierType(100, 50, 10, 2, Set(Firststrike))
  val otherType = soldierType(100, 50, 10, 2, Set.empty)
  
  test("soldier with first strike attacks first when he is attacker") {

	val attacker = new Soldier("1", firstStrikeType, Player("1"))
	val defender = new Soldier("2", otherType, Player("2"))
	attackerHex.soldier = Some(attacker)
	defenderHex.soldier = Some(defender)
	
	val result = Attack.battle(attackerHex, defenderHex, firstStrikeType.attacks(0), Some(otherType.attacks(0)), f(60))
	assert(result(0).isAttackerAttackingThisRound === true)
	assert(result(0).attacker === attacker)
	assert(result(1).isAttackerAttackingThisRound === false)
	assert(result(1).attacker === defender)
	assert(result(2).isAttackerAttackingThisRound === true)
	assert(result(2).attacker === attacker)
	assert(result(3).isAttackerAttackingThisRound === false)
	assert(result(3).attacker === defender)
  }
  
  test("soldier with first strike attacks first when he is defender") {
    val attacker = new Soldier("1", otherType, Player("1"))
	val defender = new Soldier("2", firstStrikeType, Player("2"))
	attackerHex.soldier = Some(attacker)
	defenderHex.soldier = Some(defender)
	
	val result = Attack.battle(attackerHex, defenderHex, otherType.attacks(0), Some(firstStrikeType.attacks(0)), f(60))

	assert(result(0).isAttackerAttackingThisRound === false)
	assert(result(0).attacker === defender)
	assert(result(1).isAttackerAttackingThisRound === true)
	assert(result(1).attacker === attacker)
	assert(result(2).isAttackerAttackingThisRound === false)
	assert(result(2).attacker === defender)
	assert(result(3).isAttackerAttackingThisRound === true)
	assert(result(3).attacker === attacker)
  }
  
  test("attacker attacks first when both have first strike attribute") {
    val attacker = new Soldier("1", firstStrikeType, Player("1"))
	val defender = new Soldier("2", firstStrikeType, Player("2"))
	attackerHex.soldier = Some(attacker)
	defenderHex.soldier = Some(defender)
	
	val result = Attack.battle(attackerHex, defenderHex, firstStrikeType.attacks(0), Some(firstStrikeType.attacks(0)), f(60))
	assert(result(0).isAttackerAttackingThisRound === true)
	assert(result(0).attacker === attacker)
	assert(result(1).isAttackerAttackingThisRound === false)
	assert(result(1).attacker === defender)
	assert(result(2).isAttackerAttackingThisRound === true)
	assert(result(2).attacker === attacker)
	assert(result(3).isAttackerAttackingThisRound === false)
	assert(result(3).attacker === defender)
  }
}