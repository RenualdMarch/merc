package mr.merc.map.view

import mr.merc.unit.view.SoldierView
import mr.merc.view.move.Movement
import scalafx.scene.canvas.GraphicsContext
import scala.collection.mutable.Queue

class SoldiersDrawer {
  private var _soldiers = Set[SoldierView]()
  private val _movements = Queue[Movement]()
  private def currentMovement = _movements.headOption

  def addSoldier(s: SoldierView) {
    _soldiers += s
  }

  def removeSoldier(s: SoldierView) {
    _soldiers = soldiers filterNot (_ == s)
  }

  def addMovement(movement: Movement) {
    _movements enqueue movement
  }

  def soldiers = _soldiers
  def movements = _movements

  private def drawablesInMovements = currentMovement.map(_.drawables).getOrElse(Nil)
  private def soldiersInMovements = drawablesInMovements flatMap (d => d match {
    case soldier: SoldierView => Some(soldier)
    case _ => None
  })

  def update(time: Int) {
    (_soldiers -- soldiersInMovements) foreach (_.updateTime(time))

    currentMovement match {
      case Some(move) => if (move.isOver) {
        executeAllMomentaryMovesAndStartFirstNonMomentary()
      } else {
        move.update(time)
        executeAllMomentaryMovesAndStartFirstNonMomentary()
      }
      case None => // do nothing by now
    }
  }

  def executeAllMomentaryMovesAndStartFirstNonMomentary() {
    currentMovement match {
      case Some(move) => if (!move.isStarted) {
        move.start()
      }
      case None => // do nothing
    }

    while (currentMovement.map(_.isOver).getOrElse(false)) {
      _movements.dequeue()
      currentMovement.map(_.start())
    }
  }

  def drawSoldiers(gc: GraphicsContext) {
    _soldiers -- soldiersInMovements foreach (_.drawItself(gc))
    drawablesInMovements foreach (_.drawItself(gc))
  }
}