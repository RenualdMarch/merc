package mr.merc.map.view

import mr.merc.unit.view.SoldierView
import mr.merc.view.move.Movement
import scalafx.scene.canvas.GraphicsContext
import scala.collection.mutable.Queue
import mr.merc.map.hex.view.TerrainHexView
import mr.merc.log.Logging
import scalafx.geometry.Rectangle2D
import mr.merc.view.Drawable

class SoldiersDrawer extends Logging {
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
    info(s"added movement $movement")
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
    updateAllSoldiersExceptForInMovement(time)

    currentMovement match {
      case Some(move) => if (move.isOver) {
        info(s"Movement $move is over")
        executeAllMomentaryMovesAndStartFirstNonMomentary()
      } else {
        executeAllMomentaryMovesAndStartFirstNonMomentary()
        currentMovement.foreach(_.update(time))
        executeAllMomentaryMovesAndStartFirstNonMomentary()
      }
      case None => // do nothing by now
    }
  }

  private def updateAllSoldiersExceptForInMovement(time: Int) {
    val inMovements = currentMovement match {
      case Some(move) => if (move.isOver) Nil else soldiersInMovements
      case None => Nil
    }
    (_soldiers -- inMovements) foreach (_.updateTime(time))
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

  def drawSoldiers(gc: GraphicsContext, viewRect: Rectangle2D) {
    val visibleSoldiers = soldiers.filter(_.viewRect.intersects(viewRect))
    val dirtySoldiers = visibleSoldiers.filter(_.dirtyRect.isDefined)
    val dirtyDrawables = drawablesInMovements.filter(_.dirtyRect.isDefined)
    val soldiersNotTakingPartInMovements = dirtySoldiers --
      dirtyDrawables.collect { case x: SoldierView => x }

    val drawablesToRedraw: List[Drawable] = soldiersNotTakingPartInMovements.toList ::: dirtyDrawables

    drawablesToRedraw.foreach {
      case d =>
        val rect = d.dirtyRect.get
        gc.clearRect(rect.minX, rect.minY, rect.width, rect.height)
        d.dirtyRect = None
    }

    drawablesToRedraw.foreach {
      case d =>
        d.drawItself(gc)
    }
  }
}