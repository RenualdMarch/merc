package mr.merc.map.view

import mr.merc.unit.view.SoldierView
import mr.merc.view.move.Movement
import scalafx.scene.canvas.GraphicsContext
import scala.collection.mutable.Queue

class SoldiersDrawer {
	private var _soldiers = Set[SoldierView]()
	private val _movements = Queue[Movement]()
	private var currentMovement:Option[Movement] = None
	
	def addSoldier(s:SoldierView) {
	  _soldiers += s
	}
	
	def removeSoldier(s:SoldierView) {
	  _soldiers = soldiers filterNot(_ == s)
	}
	
	def addMovement(movement:Movement) {	  
	  _movements enqueue movement
	  if (currentMovement.isEmpty) {
	    currentMovement = extractNextMovement()
	  }
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
	  
	  currentMovement match {
	    case Some(move) => if (move.isOver) {
	        _movements.dequeue()
	        currentMovement = extractNextMovement()
	        update(time)
	      } else {
	        move.update(time)
	        if (move.isOver) {
	          update(time)
	        }
	      }
	    case None => {
	      currentMovement = extractNextMovement()
	    }
	  }
	}
	
	private def extractNextMovement():Option[Movement] = if (_movements.isEmpty) {
	    None
	  } else {
	    val move = _movements.front
	    move.start()
	    Some(move)
	  }
	
	def drawSoldiers(gc:GraphicsContext) {
	  _soldiers -- soldierInMovements foreach (_.drawItself(gc))
	  drawablesInMovements foreach (_.drawItself(gc))
	}
}