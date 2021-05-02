package mr.merc.unit

import org.scalatest.FunSuite
import mr.merc.players.Player

class BeforeTurnActionTest extends FunSuite{
    val soldier1 = new Soldier("1", TestUtil.testSoldierType, Player("1"))
    val soldier2 = new Soldier("2", TestUtil.testSoldierType, Player("1"))
    val soldier3 = new Soldier("3", TestUtil.testSoldierType, Player("1"))
    
	test("1 event for each soldier remains") {
	  val event1 = new Regeneration(soldier1)
	  val event2 = new CureSoldier(soldier3, soldier2)
	  val result = BeforeTurnAction.filterActions(Set(event1, event2))
	  assert(result === Set(event1, event2))
	}
    
    test("only one healing remains") {
      val event1 = new Heal4Soldier(soldier2, soldier1)
	  val event2 = new Heal4Soldier(soldier3, soldier1)
	  val result = BeforeTurnAction.filterActions(Set(event1, event2))
	  assert(result == Set(event1) || result == Set(event2))
    }
    
    test("better healing remains") {
      val event1 = new Heal4Soldier(soldier2, soldier1)
	  val event2 = new Heal8Soldier(soldier3, soldier1)
	  val result = BeforeTurnAction.filterActions(Set(event1, event2))
	  assert(result === Set(event2))
    }
    
    test("curing more important than healing") {
      val event1 = new CureSoldier(soldier2, soldier1)
	  val event2 = new Heal8Soldier(soldier2, soldier1)
	  val result = BeforeTurnAction.filterActions(Set(event1, event2))
	  assert(result === Set(event1))
    }
    
    test("regeneration more important than healing") {
      val event1 = new CureSoldier(soldier2, soldier1)
	  val event2 = new Regeneration(soldier1)
	  val result = BeforeTurnAction.filterActions(Set(event1, event2))
	  assert(result === Set(event2))
    }
    
    test("poisoning higher than healing") {
      val event1 = new Heal4Soldier(soldier2, soldier1)
	  val event2 = new PoisoningDamage(soldier1)
	  val result = BeforeTurnAction.filterActions(Set(event1, event2))
	  assert(result === Set(event2))
    }
    
    test("poisoning lower than curing") {
      val event1 = new CureSoldier(soldier2, soldier1)
	  val event2 = new PoisoningDamage(soldier1)
	  val result = BeforeTurnAction.filterActions(Set(event1, event2))
	  assert(result === Set(event1))
    }
    
    test("doing nothing heal lower than heal4") {
      val event1 = new Heal4Soldier(soldier2, soldier1)
	  val event2 = new DoingNothingHeal2(soldier1)
	  val result = BeforeTurnAction.filterActions(Set(event1, event2))
	  assert(result === Set(event1))
    } 
    
    test("curing lower than regeneration") {
      val event1 = new Regeneration(soldier1)
	  val event2 = new PoisoningDamage(soldier1)
	  val result = BeforeTurnAction.filterActions(Set(event1, event2))
	  assert(result === Set(event1))
    }
    
    test("sanity check") {
      val event1 = new Regeneration(soldier1)
	  val event2 = new PoisoningDamage(soldier1)
      val event3 = new Heal4Soldier(soldier2, soldier1)
	  val event4 = new Heal8Soldier(soldier3, soldier1)
      val event5 = new Heal4Soldier(soldier3, soldier2)
      val event6 = new PoisoningDamage(soldier2)
      val event7 = new CureSoldier(soldier3, soldier2)
      val set = Set[BeforeTurnAction](event1, event2, event3, event4, event5, event6, event7)
      val result = BeforeTurnAction.filterActions(set)
	  assert(result === Set(event1, event7))
    }
    
    
}