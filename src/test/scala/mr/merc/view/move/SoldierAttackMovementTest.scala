package mr.merc.view.move

import org.scalatest.FunSuite
import mr.merc.unit.Soldier
import mr.merc.unit.SoldierType
import mr.merc.unit.view.SoldierView
import mr.merc.map.hex.NE
import mr.merc.unit.view.FirstAttackNESuccState

class SoldierAttackMovementTest extends FunSuite {
	test("imagesList") {
		assert(SoldierAttackMovement.imagesList(5) === List(0, 0, 0, 0, 1, 1, 1, 1,
	     2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4))
	    assert(SoldierAttackMovement.imagesList(4) === List(0, 0, 0, 0, 0, 1, 1, 1, 1, 1,
	     2, 2, 2, 2, 2, 3, 3, 3, 3, 3))
	}
	
	test("animation") {
	  val soldier = new Soldier("1", SoldierType("testSoldier2"))
	  val soldierView = new SoldierView(soldier)
	  val direction = NE
	  val from = (0, 10)
	  val to = (300, 410) 
	  // distance is 500, speed is 100, time is 5 sec
	  
	  val movement = new SoldierAttackMovement(from, to, direction,
	      true, soldierView, 0)
	  movement.start()
	  assert(soldierView.x === 0)
	  assert(soldierView.y === 10)
	  assert(soldierView.state === FirstAttackNESuccState)
	  assert(soldierView.index === 0)
	  
	  movement.update(5000 / 4)
	  assert(soldierView.x === 300 / 4)
	  assert(soldierView.y === 110)
	  assert(soldierView.state === FirstAttackNESuccState)
	  assert(soldierView.index === 1)
	  
	  movement.update(5000 / 4)
	  assert(soldierView.x === 300 / 2)
	  assert(soldierView.y === 210)
	  assert(soldierView.state === FirstAttackNESuccState)
	  assert(soldierView.index === 2)
	  
	  movement.update(5000 / 4)
	  assert(soldierView.x === 300 * 3 / 4)
	  assert(soldierView.y === 310)
	  assert(soldierView.state === FirstAttackNESuccState)
	  assert(soldierView.index === 3)
	  
	  movement.update(5000 / 4)
	  assert(soldierView.x === 300)
	  assert(soldierView.y === 410)
	  assert(soldierView.state === FirstAttackNESuccState)
	  assert(soldierView.index === 3)
	  
	  movement.update(5000 / 4)
	  assert(soldierView.x === 300 * 3 / 4)
	  assert(soldierView.y === 310)
	  assert(soldierView.state === FirstAttackNESuccState)
	  assert(soldierView.index === 3)
	  
	  movement.update(5000 / 4)
	  assert(soldierView.x === 300 / 2)
	  assert(soldierView.y === 210)
	  assert(soldierView.state === FirstAttackNESuccState)
	  assert(soldierView.index === 3)
	  
	  movement.update(5000 / 4)
	  assert(soldierView.x === 300 / 4)
	  assert(soldierView.y === 110)
	  assert(soldierView.state === FirstAttackNESuccState)
	  assert(soldierView.index === 3)
	  
	  movement.update(5000 / 4)
	  assert(soldierView.x === 0 / 2)
	  assert(soldierView.y === 10)
	  assert(soldierView.state === FirstAttackNESuccState)
	  assert(soldierView.index === 3)
	  
	  assert(movement.isOver)
	  
	}
}