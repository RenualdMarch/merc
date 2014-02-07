package mr.merc.view.move

import org.scalatest.FunSuite
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.hex.view.TerrainHexView
import mr.merc.map.hex.view.TerrainHexFieldView

class MovementListTest extends FunSuite {
  test("case with all movements momentary") {
    val list = List(new MomentaryTestMovement(), new MomentaryTestMovement, new MomentaryTestMovement)
    val move = new MovementList(list)
    move.start()
    assert(move.isOver)
    assert(list forall (_.isOver))
  }

  test("case when all movements are time taking") {
    val list = List(new TimeTakingTestMovement(10), new TimeTakingTestMovement(20), new TimeTakingTestMovement(30))
    val move = new MovementList(list)

    move.start()
    move.update(5)
    assert(!list(0).isOver)
    move.update(5)
    assert(list(0).isOver)
    assert(!list(1).isOver)
    move.update(20)
    assert(list(1).isOver)
    assert(!list(2).isOver)
    move.update(20)
    assert(!list(2).isOver)
    move.update(20)
    assert(list(2).isOver)
    assert(move.isOver)
  }

  test("both types of movement together") {
    val list = List(new TimeTakingTestMovement(10), new MomentaryTestMovement(),
      new TimeTakingTestMovement(20), new MomentaryTestMovement(),
      new TimeTakingTestMovement(30), new MomentaryTestMovement())

    val move = new MovementList(list)
    move.start()
    assert(!list(0).isOver)
    move.update(15)
    assert(list(0).isOver)
    assert(list(1).isOver)
    assert(!list(2).isOver)
    move.update(20)
    assert(list(2).isOver)
    assert(list(3).isOver)
    assert(!list(4).isOver)
    move.update(15)
    assert(!list(4).isOver)
    move.update(15)
    assert(list(4).isOver)
    assert(list(5).isOver)
    assert(move.isOver)
  }

  test("empty list") {
    val list = new MovementList(Nil)
    assert(list.isOver)
  }

}

class MomentaryTestMovement() extends Movement {
  override def update(time: Int) {
    throw new IllegalStateException("This method mustn't be called, this move is momentary")
  }
  def isOver = isStarted
}

class TimeTakingTestMovement(time: Int) extends Movement {
  private var timePassed = 0

  override def update(time: Int) {
    super.update(time)
    timePassed += time
  }
  def isOver = timePassed >= time
}