package mr.merc.view.move

import mr.merc.map.hex.view.TerrainHexView
import mr.merc.map.hex.view.TerrainHexFieldView
import mr.merc.unit.view.SoldierView
import mr.merc.unit.view.MoveState
import mr.merc.unit.sound.MovementSound

class SmoothMovement(val list: List[TerrainHexView], val soldier: SoldierView, fieldView: TerrainHexFieldView, movementSpeed: Int = 600) extends Movement {

  override def start() {
    super.start()
    updateSoldierCoords()
    soldier.state = MoveState
    soldier.sounds.get(MovementSound).foreach(_.play())
  }

  private var currentTime = 0
  val length = 72 // 72 px between to centers
  private val timeFunctions: List[MovementFunction] = {
    val timeForOneSegment = 1000 * length / movementSpeed // timeDiff in millis
    val funs = list zip list.tail map { case (f, s) => new MovementFunction(0, timeForOneSegment, f, s) }

    funs.zipWithIndex map {
      case (f, i) => f.addTime(timeForOneSegment * i)
    }
  }

  private def movementFunctionByTime(time: Int, funcs: List[MovementFunction]) = funcs.find(_.isDefinedAt(time))

  override def update(time: Int) {
    super.update(time)
    currentTime += time

    if (isOver) {
      val last = list.last
      soldier.coords = (last.x, last.y)
    } else {
      updateSoldierCoords()
    }
  }

  private def updateSoldierCoords() {
    val func = movementFunctionByTime(currentTime, timeFunctions)
    func match {
      case Some(f) =>
        val coords = f(currentTime)
        soldier.coords = coords
      case None => // do nothing, movement is over
    }
  }

  def isOver: Boolean = movementFunctionByTime(currentTime, timeFunctions).isEmpty

  override def drawables = List(soldier)
}

class MovementFunction(startT: Int, endT: Int, val startHex: TerrainHexView, val endHex: TerrainHexView) extends PartialFunction[Int, (Int, Int)] {

  private def coordByTime(t: Int): (Int, Int) = {
    require(isDefinedAt(t))
    val actualT = t - startT
    val timeLength = endT - startT
    val coveredPart = actualT / timeLength.toDouble

    val start = startHex.coords
    val end = endHex.coords
    val x = start._1 + coveredPart * (end._1 - start._1) toInt
    val y = start._2 + coveredPart * (end._2 - start._2) toInt

    (x, y)
  }

  def apply(t: Int) = coordByTime(t)

  def isDefinedAt(t: Int): Boolean = t >= startT && t <= endT

  def addTime(time: Int) = new MovementFunction(startT + time, endT + time, startHex, endHex)
}