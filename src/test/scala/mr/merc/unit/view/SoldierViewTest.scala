package mr.merc.unit.view

import org.scalatest.FunSuite
import mr.merc.map.hex._

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
}