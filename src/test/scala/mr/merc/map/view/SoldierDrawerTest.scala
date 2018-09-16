package mr.merc.map.view

import org.scalatest.FunSuite
import org.scalatest.mockito.MockitoSugar
import mr.merc.unit.view.SoldierView
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfter
import scalafx.scene.canvas.GraphicsContext
import scalafx.geometry.Rectangle2D

class SoldierDrawerTest extends FunSuite with MockitoSugar with BeforeAndAfter {
  val soldier1 = mock[SoldierView]
  val soldier2 = mock[SoldierView]
  val soldier3 = mock[SoldierView]
  val gc = mock[GraphicsContext]

  def addSoldiers(sd: SoldiersDrawer[SoldierView]) {
    sd.addSoldier(soldier1)
    sd.addSoldier(soldier2)
    sd.addSoldier(soldier3)
  }

  before {
    when(soldier1.viewRect).thenReturn(new Rectangle2D(0, 0, 10, 10))
    when(soldier2.viewRect).thenReturn(new Rectangle2D(0, 0, 10, 10))
    when(soldier3.viewRect).thenReturn(new Rectangle2D(0, 0, 10, 10))
  }

  test("simple updating without movements") {
    val soldiersDrawer = new SoldiersDrawer[SoldierView]
    addSoldiers(soldiersDrawer)

    soldiersDrawer.update(50)

    verify(soldier1, times(1)).updateTime(50)
    verify(soldier2, times(1)).updateTime(50)
    verify(soldier3, times(1)).updateTime(50)
  }

  after {
    reset(soldier1, soldier2, soldier3, gc)
  }
}
