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

  test("dirty hexes of finished movement remain once - case of time taking movement") {
    val field = new TerrainHexField(5, 5, TerrainHex.grassInit)
    val fieldView = new TerrainHexFieldView(field)
    val firstMovement = new TimeTakingTestMovement(2, List(fieldView.hex(0, 0)))
    val secondMovement = new TimeTakingTestMovement(2, List(fieldView.hex(1, 0)))

    val list = new MovementList(List(firstMovement, secondMovement))
    list.start()
    assert(list.dirtyHexes === List(fieldView.hex(0, 0)))
    list.update(1)
    assert(list.dirtyHexes === List(fieldView.hex(0, 0)))
    list.update(1)
    assert(list.dirtyHexes.toSet === Set(fieldView.hex(0, 0), fieldView.hex(1, 0)))
    list.update(1)
    assert(list.dirtyHexes === List(fieldView.hex(1, 0)))
    list.update(1)
    assert(list.dirtyHexes === List(fieldView.hex(1, 0)))
  }

  test("dirty hexes of finished movement remain once - case of momentary movement") {
    val field = new TerrainHexField(5, 5, TerrainHex.grassInit)
    val fieldView = new TerrainHexFieldView(field)
    val firstMovement = new MomentaryTestMovement(List(fieldView.hex(0, 0)))
    val secondMovement = new MomentaryTestMovement(List(fieldView.hex(1, 0)))

    val list = new MovementList(List(firstMovement, secondMovement))
    list.start()
    assert(list.dirtyHexes.toSet === Set(fieldView.hex(0, 0), fieldView.hex(1, 0)))
  }
}

class MomentaryTestMovement(override val dirtyHexes: List[TerrainHexView] = Nil) extends Movement {
  override def update(time: Int) {
    throw new IllegalStateException("This method mustn't be called, this move is momentary")
  }
  def isOver = isStarted
}

class TimeTakingTestMovement(time: Int, override val dirtyHexes: List[TerrainHexView] = Nil) extends Movement {
  private var timePassed = 0

  override def update(time: Int) {
    super.update(time)
    timePassed += time
  }
  def isOver = timePassed >= time
}