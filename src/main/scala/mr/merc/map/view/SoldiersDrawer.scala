package mr.merc.map.view

import mr.merc.view.move.Movement
import scalafx.scene.canvas.GraphicsContext

import scala.collection.mutable.{ArrayBuffer, Queue}
import mr.merc.map.hex.view.TerrainHexView
import mr.merc.log.Logging
import scalafx.geometry.Rectangle2D
import mr.merc.view.Drawable
import mr.merc.unit.view.AbstractSoldierView

import scala.reflect.ClassTag

class SoldiersDrawer[T <: AbstractSoldierView: ClassTag] extends Logging {
  private var _soldiers = Set[T]()
  private val _movements = Queue[Movement]()
  private def currentMovement = _movements.headOption
  private val rectIntersectionHelper = new RectIntersectionHelper[T]

  def addSoldier(s: T) {
    _soldiers += s
    rectIntersectionHelper.addRect(s, s.viewRect)
  }

  def removeSoldier(s: T) {
    _soldiers = soldiers filterNot (_ == s)
    rectIntersectionHelper.removeRect(s)
  }

  def addMovement(movement: Movement) {
    info(s"added movement $movement")
    _movements enqueue movement
  }

  def soldiers = _soldiers
  def movements = _movements

  private def drawablesInMovements = currentMovement.map(_.drawables).getOrElse(Nil)

  private def soldiersInMovements = drawablesInMovements flatMap (d => d match {
    case soldier: T => Some(soldier)
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

  private def calculateTouchedSoldiers(dirtySoldiers: Set[T]): Set[T] = {
    var alreadyTouched = Set[T]()
    var touchedLastTime = Set[T]()

    touchedLastTime ++= dirtySoldiers

    do {
      val touchedThisIteration = touchedLastTime.flatMap(rectIntersectionHelper.intersections)
      alreadyTouched ++= touchedLastTime
      touchedLastTime = touchedThisIteration -- alreadyTouched
    } while (touchedLastTime.nonEmpty)

    alreadyTouched -- dirtySoldiers
  }

  def drawSoldiersFromScratch(gc: GraphicsContext, viewRect: Rectangle2D) {
    gc.clearRect(0, 0, viewRect.width, viewRect.height)
    soldiers.foreach(s => s.dirtyRect = Some(s.viewRect))
    drawSoldiers(gc, viewRect)
  }

  def drawSoldiers(gc: GraphicsContext, viewRect: Rectangle2D) {
    val visibleSoldiers = soldiers.filter(_.viewRect.intersects(viewRect))
    val dirtySoldiers = visibleSoldiers.filter(_.dirtyRect.isDefined)
    val dirtyDrawables = drawablesInMovements.filter(_.dirtyRect.isDefined)
    val soldiersDrawables = dirtyDrawables.collect { case x: T => x }
    val allDirtySoldiers = dirtySoldiers ++ soldiersDrawables

    allDirtySoldiers foreach { ds =>
      rectIntersectionHelper.addRect(ds, ds.viewRect)
    }
    val touchedSoldiers = calculateTouchedSoldiers(allDirtySoldiers)
    touchedSoldiers foreach (t => t.dirtyRect = Some(t.viewRect))
    val dirtySoldiersNotTakingPartInMovements = dirtySoldiers -- soldiersDrawables
    val dirtyAndTouched = (dirtySoldiersNotTakingPartInMovements ++ touchedSoldiers).toList.sortBy(_.y)
    val drawablesToRedraw: List[Drawable] = dirtyAndTouched ::: dirtyDrawables

    drawablesToRedraw.foreach { d =>
      val rect = d.dirtyRect.get
      gc.clearRect(rect.minX - viewRect.minX, rect.minY - viewRect.minY, rect.width, rect.height)
      d.dirtyRect = None
    }

    drawablesToRedraw.foreach { d =>
      d.drawItself(gc, -viewRect.minX.toInt, -viewRect.minY.toInt)
    }
  }
}