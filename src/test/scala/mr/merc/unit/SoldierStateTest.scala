package mr.merc.unit

import org.scalatest.FunSuite
import mr.merc.players.Player

class SoldierStateTest extends FunSuite{
    val soldierType = new SoldierType("type0", 1, 20, 1, 10, 1, 
		List(), Map(), Map(), Map())
  
	test("poisoned state results in 8 hp drop") {
	  val soldier = new Soldier("1", soldierType, Player("1"))
	  soldier.addState(Poisoned)
	  soldier.hp = 10
	  soldier.endMove()
	  assert(soldier.hp === 2)
	}
	
	test("poisonined doesn't go below 1 hp") {
	  val soldier = new Soldier("1", soldierType, Player("1"))
	  soldier.addState(Poisoned)
	  soldier.hp = 8
	  soldier.endMove()
	  assert(soldier.hp === 1)
	  soldier.hp = 5
	  soldier.endMove()
	  assert(soldier.hp === 1)
	}
	
	
}