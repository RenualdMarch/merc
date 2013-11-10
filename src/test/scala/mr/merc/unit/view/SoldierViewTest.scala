package mr.merc.unit.view

import org.scalatest.FunSuite
import mr.merc.map.hex._
import mr.merc.unit._
import mr.merc.players._

class SoldierViewTest extends FunSuite {
	
	test("coords correction") {
	  val nCorr = SoldierView.coordsCorrection(N)
	  assert(nCorr === (0, -72))
	  
	  val neCorr = SoldierView.coordsCorrection(NE)
	  assert(neCorr === (54, -36))
	  
	  val nwCorr = SoldierView.coordsCorrection(NW)
	  assert(nwCorr === (-54, -36))
	  
	  val sCorr = SoldierView.coordsCorrection(S)
	  assert(sCorr === (0, 72))
	  
	  val seCorr = SoldierView.coordsCorrection(SE)
	  assert(seCorr === (54, 36))
	  
	  val swCorr = SoldierView.coordsCorrection(SW)
	  assert(swCorr === (-54, 36))
	}
	
	test("after setting state to death after animation is finished, noState is used") {
	  val soldierType = SoldierType("testSoldier")
	  val soldier = new Soldier("1", soldierType, Player("1"))
	  val view = new SoldierView(soldier)
	  
	  view.state = StandState
	  view.updateTime(10000)
	  view.state = DeathState
	  view.updateTime(10000)
	  assert(view.state === NoState)
	}
	
	test("after setting idle state after animation is over, standing state is used") {
	  val soldierType = SoldierType("testSoldier")
	  val soldier = new Soldier("1", soldierType, Player("1"))
	  val view = new SoldierView(soldier)
	  
	  view.state = StandState
	  view.updateTime(10000)
	  view.state = IdleState
	  view.updateTime(10000)
	  assert(view.state === StandState)
	}
}