package mr.merc.map.view

import mr.merc.unit.view.SoldierView
import mr.merc.view.move.Movement
import scalafx.scene.canvas.GraphicsContext

class SoldiersDrawer {
	private var _soldiers = Set[SoldierView]()
	private var _movements = List[Movement]()
	
	def addSoldier(s:SoldierView) {
	  _soldiers += s
	}
	
	def removeSoldier(s:SoldierView) {
	  _soldiers = soldiers filterNot(_ == s)
	}
	
	def addMovement(movement:Movement) {
	  _movements ::= movement
	}
	
	def soldiers = _soldiers
	def movements = _movements
	
	private def soldierInMovements = movements flatMap (_.soldiers)
	
	def update(time:Int) {
	  _soldiers foreach (_.updateTime(time))
	  _movements foreach (_.update(time))
	  _movements = _movements filterNot(_.isOver)
	}
	
	def drawSoldiers(gc:GraphicsContext) {
	  _soldiers -- soldierInMovements foreach (_.drawItself(gc))
	  soldierInMovements foreach (_.drawItself(gc))
	}
}