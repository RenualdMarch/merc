package mr.merc.map.view

import mr.merc.unit.view.SoldierView
import mr.merc.view.move.Movement
import scalafx.scene.canvas.GraphicsContext
import scala.collection.mutable.Queue

class SoldiersDrawer {
	private var _soldiers = Set[SoldierView]()
	private val _movements = Queue[Movement]()
	
	def addSoldier(s:SoldierView) {
	  _soldiers += s
	}
	
	def removeSoldier(s:SoldierView) {
	  _soldiers = soldiers filterNot(_ == s)
	}
	
	def addMovement(movement:Movement) {
	  movement.start()
	  _movements enqueue movement
	}
	
	def soldiers = _soldiers
	def movements = _movements
	
	private def drawablesInMovements = movements.headOption.map(_.drawables).getOrElse(Nil)
	private def soldierInMovements = drawablesInMovements flatMap(d => d match {
	  case soldier:SoldierView => Some(soldier)
	  case _ => None
 	})
	
	def update(time:Int) {
	  _soldiers foreach (_.updateTime(time))
	  if (!_movements.isEmpty) {
	  _movements.front.update(time)
	    if (movements.front.isOver) {
	      _movements.dequeue()
	    }
	  }
	}
	
	def drawSoldiers(gc:GraphicsContext) {
	  _soldiers -- soldierInMovements foreach (_.drawItself(gc))
	  drawablesInMovements foreach (_.drawItself(gc))
	}
}