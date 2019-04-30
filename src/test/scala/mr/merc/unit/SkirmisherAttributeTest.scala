package mr.merc.unit
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain.{GrassKind, GreenGrass}
import mr.merc.players.Player
import mr.merc.map.GameField
import mr.merc.battle.event.AttackModelEvent
import mr.merc.battle.event.AttackModelEventResult
import mr.merc.battle.event.MovementModelEvent
import mr.merc.battle.event.MovementModelEventResult
import mr.merc.battle.BattleModel
import mr.merc.battle.event.MovementModelEvent

class SkirmisherAttributeTest extends FunSuite with BeforeAndAfter {
  var field: TerrainHexField = _
  var model: BattleModel = _
  var soldier: Soldier = _

  before {
    field = new TerrainHexField(10, 10, (x, y) => new TerrainHex(x, y, GreenGrass))
    model = new BattleModel(new GameField(field, List(Player("1"), Player("2"))))
    val enemy = new Soldier("2", simpleSoldierType(Set()), Player("2"))
    field.hex(1, 0).soldier = Some(enemy)
    soldier = new Soldier("1", simpleSoldierType(Set(Skirmisher)), Player("1"))
    field.hex(0, 0).soldier = Some(soldier)
  }

  def simpleSoldierType(attributes: Set[SoldierTypeAttribute] = Set()) = new SoldierType("1", 1, 20, 6, 5, 1,
    List(Attack(1, 10, 1, Impact, false), Attack(2, 6, 2, Impact, false)), Map(GrassKind -> 2),
    Map(GrassDefence -> 60), Map(Impact -> 0), attributes, viewName = "")

  test("possible moves are correct with skirmisher soldier") {
    val currentField = field
    import currentField._
    val moves = model.possibleMoves(soldier, hex(0, 0))
    assert(moves === Set(hex(0, 1), hex(1, 1), hex(0, 2), hex(2, 1), hex(2, 2), hex(1, 2), hex(0, 3)))
  }

  test("validation is correct with skirmisher soldier") {
    val currentField = field
    import currentField._
    assert(model.validateMovementEvent(soldier, hex(0, 0), hex(2, 1), true, false) === true)
  }

  test("actual movement by skirmisher soldier can be performed") {
    val event = new MovementModelEvent(soldier, field.hex(0, 0), field.hex(2, 1))
    model.handleEvent(event)
    assert(field.hex(0, 0).soldier === None)
    assert(field.hex(2, 1).soldier === Some(soldier))
  }

}