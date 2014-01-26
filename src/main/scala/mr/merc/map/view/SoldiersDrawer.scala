package mr.merc.map.view

import mr.merc.unit.view.SoldierView
import mr.merc.view.move.Movement
import scalafx.scene.canvas.GraphicsContext
import scala.collection.mutable.Queue
import mr.merc.map.hex.view.TerrainHexView
import mr.merc.log.Logging

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

  private var momentaryAndPreviousDirtyHexes: List[TerrainHexView] = Nil
  private def drawablesInMovements = currentMovement.map(_.drawables).getOrElse(Nil)
  def dirtyHexesInMovements = {
    momentaryAndPreviousDirtyHexes ++ currentMovement.map(_.dirtyHexes).getOrElse(Nil)
  }
  private def soldiersInMovements = drawablesInMovements flatMap (d => d match {
    case soldier: SoldierView => Some(soldier)
    case _ => None
  })

  def update(time: Int) {
    momentaryAndPreviousDirtyHexes = Nil
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
      momentaryAndPreviousDirtyHexes ++= currentMovement.get.dirtyHexes
      _movements.dequeue()
      currentMovement.map(_.start())
    }
  }

  def drawDrawablesInMovements(gc: GraphicsContext) {
    drawablesInMovements foreach (_.drawItself(gc))
  }

  def drawSoldiers(gc: GraphicsContext, hexes: List[TerrainHexView]) {
    hexes.flatMap(_.soldier).foreach(_.drawItself(gc))
  }
}