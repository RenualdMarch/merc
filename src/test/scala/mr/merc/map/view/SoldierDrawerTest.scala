package mr.merc.map.view

import org.scalatest.FunSuite
import org.scalatest.mock.MockitoSugar
import mr.merc.unit.view.SoldierView
import mr.merc.view.move.Movement
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfter
import scalafx.scene.canvas.GraphicsContext

class SoldierDrawerTest extends FunSuite with MockitoSugar with BeforeAndAfter {
 val soldier1 = mock[SoldierView]
 val soldier2 = mock[SoldierView]
 val soldier3 = mock[SoldierView]
 val gc = mock[GraphicsContext]
 
 def addSoldiers(sd:SoldiersDrawer) {
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
   soldiersDrawer.drawSoldiers(gc)
   val inOrder1 = org.mockito.Mockito.inOrder(soldier1, soldier3, soldier2);
   inOrder1.verify(soldier1).drawItself(gc)
   inOrder1.verify(soldier3).drawItself(gc)
   inOrder1.verify(soldier2).drawItself(gc)
   
   soldiersDrawer.update(50)
   assert(soldiersDrawer.movements === List(move))
   
   soldiersDrawer.update(50)
   assert(soldiersDrawer.movements === Nil)
   soldiersDrawer.drawSoldiers(gc)
   val inOrder2 = org.mockito.Mockito.inOrder(soldier1, soldier3, soldier2);
   inOrder1.verify(soldier1).drawItself(gc)
   inOrder1.verify(soldier2).drawItself(gc)
   inOrder1.verify(soldier3).drawItself(gc)
 }
 
 test("movement with two soldiers") {
   val soldiersDrawer = new SoldiersDrawer
   addSoldiers(soldiersDrawer)
   
   val move = new ExampleMovement(List(soldier1, soldier2))
   soldiersDrawer.addMovement(move)
   soldiersDrawer.drawSoldiers(gc)
   val inOrder = org.mockito.Mockito.inOrder(soldier1, soldier3, soldier2);
   inOrder.verify(soldier3).drawItself(gc)
   inOrder.verify(soldier1).drawItself(gc)
   inOrder.verify(soldier2).drawItself(gc)
 }
 
 test("two separate movements with 1 soldier each") {
   val soldiersDrawer = new SoldiersDrawer
   addSoldiers(soldiersDrawer)
   
   val move1 = new ExampleMovement(List(soldier1))
   soldiersDrawer.addMovement(move1)
   val move2 = new ExampleMovement(List(soldier3))
   soldiersDrawer.addMovement(move2)
   
   assert(soldiersDrawer.movements === List(move2, move1))
   
   soldiersDrawer.drawSoldiers(gc)
   val inOrder = org.mockito.Mockito.inOrder(soldier1, soldier3, soldier2);
   inOrder.verify(soldier2).drawItself(gc)
   inOrder.verify(soldier3).drawItself(gc)
   inOrder.verify(soldier1).drawItself(gc)
 }
 
 after {
   reset(soldier1, soldier2, soldier3, gc)
 }
}

class ExampleMovement(override val soldiers:List[SoldierView]) extends Movement {
    private var updatedCount = 0
  
    def start() {}
    
    def update(time:Int) {
      updatedCount += 1
    }
    
	def isOver = updatedCount == 2

}