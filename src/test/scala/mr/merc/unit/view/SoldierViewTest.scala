package mr.merc.unit.view

import org.scalatest.FunSuite
import mr.merc.map.hex._
import mr.merc.unit._
import mr.merc.players._
import org.scalatest.BeforeAndAfter
import mr.merc.view.Sprite

class SoldierViewTest extends FunSuite {

  test("coords correction") {
    val nCorr = SoldierView.coordsCorrection(N, 1.0)
    assert(nCorr === (0, -72))

    val neCorr = SoldierView.coordsCorrection(NE, 1.0)
    assert(neCorr === (54, -36))

    val nwCorr = SoldierView.coordsCorrection(NW, 1.0)
    assert(nwCorr === (-54, -36))

    val sCorr = SoldierView.coordsCorrection(S, 1.0)
    assert(sCorr === (0, 72))

    val seCorr = SoldierView.coordsCorrection(SE, 1.0)
    assert(seCorr === (54, 36))

    val swCorr = SoldierView.coordsCorrection(SW, 1.0)
    assert(swCorr === (-54, 36))
  }

  test("after setting state to death after animation is finished, noState is used") {
    val soldierType = TestUtil.testSoldierType
    val soldier = new Soldier("1", soldierType, Player("1"))
    val view = new SoldierView(soldier, 1.0)

    view.state = StandState
    view.updateTime(10000)
    view.state = DeathState
    view.updateTime(10000)
    view.updateTime(10000)
    view.updateTime(10000)
    view.updateTime(10000)
    assert(view.state === NoState)
  }

  test("after setting idle state after animation is over, standing state is used") {
    val soldierType = TestUtil.testSoldierType
    val soldier = new Soldier("1", soldierType, Player("1"))
    val view = new SoldierView(soldier, 1.0)

    view.state = StandState
    view.updateTime(10000)
    view.state = IdleState
    view.updateTime(10000)
    assert(view.state === StandState)
  }
}