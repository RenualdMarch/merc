package mr.merc.battle

import org.scalatest.FunSuite
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.GameField
import mr.merc.map.hex.TerrainHex
import mr.merc.players.Player
import mr.merc.map.terrain.Grass
import org.scalatest.BeforeAndAfter
import mr.merc.unit.SoldierType
import mr.merc.unit.Soldier
import org.scalatest.mock.MockitoSugar
import mr.merc.map.view.SoldiersDrawer
import org.mockito.Mockito._
import mr.merc.battle.event.MoveBattleViewEvent
import org.hamcrest.Matcher
import org.hamcrest.BaseMatcher
import org.mockito.ArgumentMatcher
import mr.merc.view.move.MovementList
import mr.merc.view.move.SoldierMoveMovement
import mr.merc.unit.view.SoldierView
import mr.merc.unit.Attack
import mr.merc.unit.Impact
import mr.merc.unit.AttackResult
import mr.merc.battle.event.AttackBattleViewEvent
import mr.merc.view.move.SoldierRangedAttackMovement
import mr.merc.map.hex.SE
import mr.merc.view.move.SoldierAttackMovement
import mr.merc.map.hex.NW
import mr.merc.view.move.MomentaryMovement

class BattleViewTest extends FunSuite with BeforeAndAfter with MockitoSugar {
  val field = new TerrainHexField(10, 10, (x, y) => new TerrainHex(x, y, Grass))
  val model = new BattleModel(new GameField(field, List(Player("1"), Player("2"))))
  val simpleSoldierType = new SoldierType("testType1", 1, 20, 10, 5, 1,
    List(), Map(Grass -> 2), Map(), Map())
  val rangedSoldierType = new SoldierType("testType1", 1, 20, 10, 5, 1,
    List(Attack(0, 1, 2, Impact, true)), Map(), Map(), Map())
  val closedSoldierType = new SoldierType("testType1", 1, 20, 10, 5, 1,
    List(Attack(0, 1, 2, Impact, false)), Map(), Map(), Map())

  val soldierDrawer = mock[SoldiersDrawer]

  var view: BattleView = _

  after {
    reset(soldierDrawer)
    view = null
  }

  test("initial soldier views have correct coords") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    val field = model.map.hexField
    val from = field.hex(1, 0)
    from.soldier = Some(soldier)
    view = new BattleView(model)
    val soldierView = view.wrap(soldier)
    assert(soldierView.x === 72 * 3 / 4)
    assert(soldierView.y === 72 / 2)
  }

  test("movement event") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    when(soldierDrawer.soldiers).thenReturn(Set(new SoldierView(soldier)))
    val field = model.map.hexField
    val from = field.hex(0, 0)
    from.soldier = Some(soldier)
    view = new BattleView(model, soldierDrawer)

    view.handleEvent(MoveBattleViewEvent(soldier, List(field.hex(0, 0), field.hex(1, 0), field.hex(2, 0))))

    verify(soldierDrawer).addMovement(org.mockito.Matchers.argThat(new ArgumentMatcher {
      def matches(argument: Any): Boolean = {
        argument match {
          case moves: MovementList => {
            assert(moves.list.size === 3)
            val first = moves.list(0).asInstanceOf[SoldierMoveMovement]
            assert(first.from.hex === field.hex(0, 0))
            assert(first.to.hex === field.hex(1, 0))
            assert(first.soldier.soldier === soldier)
            val second = moves.list(1).asInstanceOf[SoldierMoveMovement]
            assert(second.from.hex === field.hex(1, 0))
            assert(second.to.hex === field.hex(2, 0))
            assert(second.soldier.soldier === soldier)
            val third = moves.list(2).asInstanceOf[MomentaryMovement]
            true
          }
          case _ => false
        }
      }
    }))
  }

  test("attack ranged") {
    val attacker = new Soldier("1", rangedSoldierType, Player("1"))
    val defender = new Soldier("2", closedSoldierType, Player("2"))
    val result = List(AttackResult(true, attacker, defender, rangedSoldierType.attacks(0), true, 10, 0),
      AttackResult(true, attacker, defender, rangedSoldierType.attacks(0), false, 10, 0))
    val field = model.map.hexField
    val from = field.hex(0, 0)
    from.soldier = Some(attacker)
    val to = field.hex(1, 0)
    to.soldier = Some(defender)
    val soldierDrawer = new SoldiersDrawer
    view = new BattleView(model, soldierDrawer)
    val event = new AttackBattleViewEvent(from, to, attacker, defender, result)
    view.handleEvent(event)
    assert(soldierDrawer.movements.size === 2) // 1 is hackish refresh
    assert(soldierDrawer.movements(0).isInstanceOf[MovementList])
    val move = soldierDrawer.movements(0).asInstanceOf[MovementList]
    assert(move.list.size === 4)

    val first = move.list(0).asInstanceOf[SoldierRangedAttackMovement]
    assert(first.from === (0, 0))
    assert(first.to === (72 * 3 / 4, 72 / 2))
    assert(first.attacker.soldier === attacker)
    assert(first.defender.soldier === defender)
    assert(first.dir === SE)

    val second = move.list(1).asInstanceOf[SoldierRangedAttackMovement]
    assert(second.from === (0, 0))
    assert(second.to === (72 * 3 / 4, 72 / 2))
    assert(second.attacker.soldier === attacker)
    assert(second.defender.soldier === defender)
    assert(second.dir === SE)
  }

  test("attack not ranged") {
    val attacker = new Soldier("1", closedSoldierType, Player("1"))
    val defender = new Soldier("2", closedSoldierType, Player("2"))
    val result = List(AttackResult(true, attacker, defender, closedSoldierType.attacks(0), false, 20, 0),
      AttackResult(false, defender, attacker, closedSoldierType.attacks(0), true, 5, 0))
    val field = model.map.hexField
    val from = field.hex(0, 0)
    from.soldier = Some(attacker)
    val to = field.hex(1, 0)
    to.soldier = Some(defender)
    val soldierDrawer = new SoldiersDrawer
    view = new BattleView(model, soldierDrawer)

    val event = new AttackBattleViewEvent(from, to, attacker, defender, result)
    view.handleEvent(event)

    assert(soldierDrawer.movements.size === 2) // 1 is hackish refresh
    assert(soldierDrawer.movements(0).isInstanceOf[MovementList])
    val move = soldierDrawer.movements(0).asInstanceOf[MovementList]
    assert(move.list.size === 4)

    val first = move.list(0).asInstanceOf[SoldierAttackMovement]
    assert(first.fromHex.coords === (0, 0))
    assert(first.toHex.coords === (72 * 3 / 4, 72 / 2))
    assert(first.attacker.soldier === attacker)
    assert(first.defender.soldier === defender)
    assert(first.dir === SE)
    assert(first.result.success === false)
    assert(first.result.attackIndex === 0)

    val second = move.list(1).asInstanceOf[SoldierAttackMovement]
    assert(second.fromHex.coords === (72 * 3 / 4, 72 / 2))
    assert(second.toHex.coords === (0, 0))
    assert(second.attacker.soldier === defender)
    assert(second.defender.soldier === attacker)
    assert(second.dir === NW)
    assert(second.result.success === true)
    assert(second.result.attackIndex === 0)

  }
}