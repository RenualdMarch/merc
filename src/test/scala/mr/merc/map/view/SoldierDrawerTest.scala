package mr.merc.map.view

import org.scalatest.FunSuite
import org.scalatest.mock.MockitoSugar
import mr.merc.unit.view.SoldierView
import mr.merc.view.move.Movement
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfter
import scalafx.scene.canvas.GraphicsContext
import mr.merc.map.hex.view.TerrainHexView
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.hex.view.TerrainHexFieldView
import mr.merc.view.move.MomentaryMovement

class SoldierDrawerTest extends FunSuite with MockitoSugar with BeforeAndAfter {
  val soldier1 = mock[SoldierView]
  val soldier2 = mock[SoldierView]
  val soldier3 = mock[SoldierView]
  val gc = mock[GraphicsContext]

  def addSoldiers(sd: SoldiersDrawer) {
    sd.addSoldier(soldier1)
    sd.addSoldier(soldier2)
    sd.addSoldier(soldier3)
  }

  test("simple updating without movements") {
    val soldiersDrawer = new SoldiersDrawer
    addSoldiers(soldiersDrawer)

    soldiersDrawer.update(50)

    verify(soldier1, times(1)).updateTime(50)
    verify(soldier2, times(1)).updateTime(50)
    verify(soldier3, times(1)).updateTime(50)
  }

  test("movement with one soldier") {
    val soldiersDrawer = new SoldiersDrawer
    addSoldiers(soldiersDrawer)

    val move = new ExampleMovement(List(soldier2))
    soldiersDrawer.addMovement(move)

    assert(soldiersDrawer.movements === List(move))
    soldiersDrawer.drawDrawablesInMovements(gc)
    soldiersDrawer.update(50)
    assert(soldiersDrawer.movements === List(move))

    soldiersDrawer.update(50)
    assert(soldiersDrawer.movements === Nil)
    soldiersDrawer.drawDrawablesInMovements(gc)
    verify(soldier2, times(1)).drawItself(gc)
    verify(soldier1, never()).drawItself(gc)
    verify(soldier3, never()).drawItself(gc)
  }

  test("movement with two soldiers") {
    val soldiersDrawer = new SoldiersDrawer
    addSoldiers(soldiersDrawer)

    val move = new ExampleMovement(List(soldier1, soldier2))
    soldiersDrawer.addMovement(move)
    soldiersDrawer.drawDrawablesInMovements(gc)
    val inOrder = org.mockito.Mockito.inOrder(soldier1, soldier2);
    inOrder.verify(soldier1).drawItself(gc)
    inOrder.verify(soldier2).drawItself(gc)
    verify(soldier3, never()).drawItself(gc)
  }

  test("two separate movements with 1 soldier each") {
    val soldiersDrawer = new SoldiersDrawer
    addSoldiers(soldiersDrawer)

    val move1 = new ExampleMovement(List(soldier1, soldier2))
    val move2 = new ExampleMovement(List(soldier3))
    soldiersDrawer.addMovement(move1)
    soldiersDrawer.addMovement(move2)

    assert(soldiersDrawer.movements.toList === List(move1, move2))

    soldiersDrawer.drawDrawablesInMovements(gc)
    val inOrder = org.mockito.Mockito.inOrder(soldier1, soldier2, soldier3);
    inOrder.verify(soldier1).drawItself(gc)
    inOrder.verify(soldier2).drawItself(gc)
    verify(soldier3, never()).drawItself(gc)
  }

  test("dirty hexes are saved when movement is over") {
    val hexField = new TerrainHexField(5, 5, TerrainHex.grassInit)
    val hexFieldView = new TerrainHexFieldView(hexField)
    val hex1 = hexFieldView.hex(1, 1)
    val hex2 = hexFieldView.hex(2, 2)
    val movement1 = new ExampleMovement(Nil, List(hex1))
    val movement2 = new ExampleMovement(Nil, List(hex2))
    val soldiersDrawer = new SoldiersDrawer
    soldiersDrawer.addMovement(movement1)
    soldiersDrawer.addMovement(movement2)

    assert(soldiersDrawer.dirtyHexesInMovements === List(hex1))
    soldiersDrawer.update(10)
    assert(soldiersDrawer.dirtyHexesInMovements === List(hex1))
    soldiersDrawer.update(10)
    assert(soldiersDrawer.dirtyHexesInMovements === List(hex1, hex2))
    soldiersDrawer.update(10)
    assert(soldiersDrawer.dirtyHexesInMovements === List(hex2))
    soldiersDrawer.update(10)
    assert(soldiersDrawer.dirtyHexesInMovements === List(hex2))
    soldiersDrawer.update(10)
    assert(soldiersDrawer.dirtyHexesInMovements === Nil)
  }

  test("dirty hexes are saved when movement is momentary") {
    val hexField = new TerrainHexField(5, 5, TerrainHex.grassInit)
    val hexFieldView = new TerrainHexFieldView(hexField)
    val hex1 = hexFieldView.hex(1, 1)
    val hex2 = hexFieldView.hex(2, 2)
    val movement1 = new ExampleMomentaryMovement(List(hex1))
    val movement2 = new ExampleMovement(Nil, List(hex2))
    val soldiersDrawer = new SoldiersDrawer
    soldiersDrawer.addMovement(movement1)
    soldiersDrawer.addMovement(movement2)
    assert(soldiersDrawer.dirtyHexesInMovements === List(hex1))
    soldiersDrawer.update(10)
    assert(soldiersDrawer.dirtyHexesInMovements === List(hex1, hex2))
    soldiersDrawer.update(10)
    assert(soldiersDrawer.dirtyHexesInMovements === List(hex2))
  }

  after {
    reset(soldier1, soldier2, soldier3, gc)
  }
}

class ExampleMovement(override val drawables: List[SoldierView], override val dirtyHexes: List[TerrainHexView] = Nil) extends Movement {
  private var updatedCount = 0

  override def update(time: Int) {
    super.update(time)
    updatedCount += 1
  }

  def isOver = updatedCount == 2
}

class ExampleMomentaryMovement(override val dirtyHexes: List[TerrainHexView] = Nil) extends MomentaryMovement(Unit, dirtyHexes) {

}