package mr.merc.battle

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import mr.merc.unit._
import mr.merc.players.Player
import mr.merc.map.GameField
import mr.merc.battle.event.AttackModelEvent
import mr.merc.battle.event.AttackModelEventResult
import mr.merc.battle.event.MovementModelEvent
import mr.merc.battle.event.MovementModelEventResult
import mr.merc.map.objects.House._
import mr.merc.map.objects.WoodenBridge

class BattleModelTest extends FunSuite with BeforeAndAfter {
  var field: TerrainHexField = _
  val simpleSoldierType = new SoldierType("1", 1, 20, 10, 5, 1,
    List(Attack(1, 10, 1, Impact, false), Attack(2, 6, 2, Impact, false)), Map(GrassKind -> 2, WallsKind -> 3, SandKind -> 5),
    Map(GrassDefence -> 60), Map(Impact -> 0), viewName = "")
  var model: BattleModel = _

  before {
    field = new TerrainHexField(10, 10, (x, y) => new TerrainHex(x, y, GreenGrass))
    model = new BattleModel(new GameField(field, List(Player("1"), Player("2")), Set(Set(Player("1")), Set(Player("2")))))
  }

  test("movement validation") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    field.hex(0, 0).soldier = Some(soldier)
    val anotherSoldierOfThisPlayer = new Soldier("2", simpleSoldierType, Player("1"))
    val anotherSoldierOfAnotherPlayer = new Soldier("3", simpleSoldierType, Player("2"))
    field.hex(2, 1).soldier = Some(anotherSoldierOfThisPlayer)
    field.hex(0, 4).soldier = Some(anotherSoldierOfAnotherPlayer)

    assert(model.validateMovementEvent(soldier, field.hex(0, 0), field.hex(1, 1)) === true)
    assert(model.validateMovementEvent(soldier, field.hex(0, 0), field.hex(5, 2)) === true)
    assert(model.validateMovementEvent(soldier, field.hex(0, 0), field.hex(6, 3)) === false)
    assert(model.validateMovementEvent(soldier, field.hex(0, 0), field.hex(0, 0)) === false)
    assert(model.validateMovementEvent(soldier, field.hex(0, 0), field.hex(2, 1)) === false)
    assert(model.validateMovementEvent(soldier, field.hex(0, 0), field.hex(0, 4)) === false)
    assert(model.validateMovementEvent(soldier, field.hex(0, 0), field.hex(0, 5)) === false)

    // cann't move when already attacked
    soldier.attackedThisTurn = true
    assert(model.validateMovementEvent(soldier, field.hex(0, 0), field.hex(1, 1)) === false)
    soldier.attackedThisTurn = false

    // cann't move when already moved and is in enemy's zone of control
    soldier.movePointsRemain -= 1

    val oneMoreEnemy = new Soldier("5", simpleSoldierType, Player("2"))
    field.hex(1, 0).soldier = Some(oneMoreEnemy)
    assert(model.validateMovementEvent(soldier, field.hex(0, 0), field.hex(0, 1)) === false)

    // can move when is in enemy's zone of control and havn't moved
    val mover = new Soldier("6", simpleSoldierType, Player("1"))
    val enemy1 = new Soldier("6", simpleSoldierType, Player("2"))
    val enemy2 = new Soldier("7", simpleSoldierType, Player("2"))
    field.hex(6, 4).soldier = Some(mover)
    field.hex(5, 3).soldier = Some(enemy1)
    field.hex(7, 4).soldier = Some(enemy2)
    assert(model.validateMovementEvent(mover, field.hex(6, 4), field.hex(6, 3)) === true)
    assert(model.validateMovementEvent(mover, field.hex(6, 4), field.hex(7, 3)) === true)
    assert(model.validateMovementEvent(mover, field.hex(6, 4), field.hex(6, 5)) === true)
    assert(model.validateMovementEvent(mover, field.hex(6, 4), field.hex(5, 4)) === true)
    assert(model.validateMovementEvent(mover, field.hex(6, 4), field.hex(7, 2)) === false)
  }

  test("attack validation") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    val ally1 = new Soldier("2", simpleSoldierType, Player("1"))
    val enemy1 = new Soldier("3", simpleSoldierType, Player("2"))
    val enemy2 = new Soldier("4", simpleSoldierType, Player("2"))
    field.hex(0, 0).soldier = Some(soldier)
    field.hex(0, 1).soldier = Some(enemy1)
    field.hex(1, 0).soldier = Some(ally1)
    field.hex(1, 1).soldier = Some(enemy2)

    assert(model.validateAttackEvent(soldier, field.hex(0, 0), field.hex(0, 1), 3) === false)
    assert(model.validateAttackEvent(soldier, field.hex(0, 0), field.hex(0, 1), 0) === true)
    assert(model.validateAttackEvent(soldier, field.hex(0, 0), field.hex(1, 0), 0) === false)
    assert(model.validateAttackEvent(soldier, field.hex(0, 0), field.hex(1, 1), 0) === false)
  }

  // TODO move this test to its class, cause here Attack class is under test
  test("defender weapon selection") {
    val attackerSoldierType = new SoldierType("1", 1, 50, 10, 5, 1,
      List(Attack(1, 10, 1, Impact, false), Attack(2, 20, 1, Impact, true)),
      Map(GrassKind -> 2), Map(GrassDefence -> 60), Map(Impact -> 0), viewName = "")

    val defenderSoldierTypeOnlyCloseCombat = new SoldierType("2", 1, 50, 10, 5, 1,
      List(Attack(1, 10, 1, Impact, false), Attack(2, 6, 2, Impact, false),
        Attack(3, 3, 3, Impact, false)),
      Map(GrassKind -> 2), Map(GrassDefence -> 60), Map(Impact -> 0), viewName = "")

    val defenderSoldierTypeOnlyOneAttack = new SoldierType("3", 1, 50, 10, 5, 1,
      List(Attack(1, 10, 1, Impact, true)),
      Map(GrassKind -> 2), Map(GrassDefence -> 60), Map(Impact -> 0), viewName = "")

    val attacker = new Soldier("1", attackerSoldierType, Player("1"))
    val defenderCloseCombat = new Soldier("2", defenderSoldierTypeOnlyCloseCombat, Player("2"))
    val defenderOnlyOneAttack = new Soldier("3", defenderSoldierTypeOnlyOneAttack, Player("3"))
    val closeAttack = attackerSoldierType.attacks(0)
    val rangedAttack = attackerSoldierType.attacks(1)

    val bestAttack = Attack.selectBestAttackForDefender(attacker, defenderCloseCombat, closeAttack)
    assert(bestAttack.get === defenderCloseCombat.soldierType.attacks(1))

    val noAttack = Attack.selectBestAttackForDefender(attacker, defenderCloseCombat, rangedAttack)
    assert(noAttack === None)

    val onlyAttack = Attack.selectBestAttackForDefender(attacker, defenderOnlyOneAttack, rangedAttack)
    assert(onlyAttack.get === defenderOnlyOneAttack.soldierType.attacks(0))

    val noAttack2 = Attack.selectBestAttackForDefender(attacker, defenderOnlyOneAttack, closeAttack)
    assert(noAttack2 === None)
  }

  test("attack event") {
    val attacker = new Soldier("1", simpleSoldierType, Player("1"))
    val defender = new Soldier("2", simpleSoldierType, Player("2"))
    field.hex(0, 0).soldier = Some(attacker)
    field.hex(1, 0).soldier = Some(defender)

    val event = new AttackModelEvent(attacker, field.hex(0, 0), field.hex(1, 0), 0)

    val result = model.handleEvent(event)
    result match {
      case AttackModelEventResult(attackerTerrainHex, defenterTerrainHex,
        attacker, defender, result) => {
        assert(attackerTerrainHex === field.hex(0, 0))
        assert(defenterTerrainHex === field.hex(1, 0))
        assert(result.size === 3)
        assert(result(0).attacker === attacker)
        assert(result(0).defender === defender)
        assert(result(0).attackersAttack === simpleSoldierType.attacks(0))
        assert(result(1).attacker === defender)
        assert(result(1).defender === attacker)
        assert(result(1).attackersAttack === simpleSoldierType.attacks(1))
        assert(result(2).attacker === defender)
        assert(result(2).defender === attacker)
        assert(result(2).attackersAttack === simpleSoldierType.attacks(1))

        assert(attacker.attackedThisTurn === true)
        assert(defender.attackedThisTurn === false)
      }
      case _ => fail
    }
  }

  test("move event") {
    val mover = new Soldier("1", simpleSoldierType, Player("1"))
    field.hex(0, 0).soldier = Some(mover)

    val event = MovementModelEvent(mover, field.hex(0, 0), field.hex(2, 1))
    val result = model.handleEvent(event)

    result match {
      case MovementModelEventResult(moverFromEvent, path) => {
        assert(mover === moverFromEvent)
        assert(path === List(field.hex(0, 0), field.hex(1, 0), field.hex(2, 1)))
        assert(mover.movePointsRemain === 6)
        assert(field.hex(0, 0).soldier === None)
        assert(field.hex(2, 1).soldier === Some(mover))
      }
      case _ => fail
    }
  }

  test("possible moves") {
    val simpleSoldierType = new SoldierType("1", 1, 20, 4, 5, 1,
      List(), Map(GrassKind -> 2),
      Map(), Map(), viewName = "")
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    field.hex(0, 0).soldier = Some(soldier)
    val moves = model.possibleMoves(soldier, field.hex(0, 0))
    val currentField = field
    import currentField.hex
    assert(moves === Set(hex(0, 1), hex(1, 0), hex(2, 0), hex(2, 1), hex(1, 1), hex(0, 2)))
  }

  test("possible attacks when there are no moves should return empty set when there are moves") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    field.hex(0, 0).soldier = Some(soldier)
    val enemy = new Soldier("2", simpleSoldierType, Player("2"))
    field.hex(0, 1).soldier = Some(enemy)
    val moves = model.possibleAttacksWhenThereAreNoMoves(soldier, field.hex(0, 0))
    assert(moves.size === 0)
  }

  test("possible attacks when there are no moves should return empty set when there are no moves and no enemies near") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    soldier.movePointsRemain = 0
    field.hex(0, 0).soldier = Some(soldier)
    val enemy = new Soldier("2", simpleSoldierType, Player("2"))
    field.hex(0, 2).soldier = Some(enemy)
    val moves = model.possibleAttacksWhenThereAreNoMoves(soldier, field.hex(0, 0))
    assert(moves.size === 0)
  }

  test("possible attacks when there are no moves should return enemies that are near when there are moves") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    soldier.movePointsRemain = 0
    field.hex(0, 0).soldier = Some(soldier)
    val enemy = new Soldier("2", simpleSoldierType, Player("2"))
    field.hex(0, 1).soldier = Some(enemy)
    val moves = model.possibleAttacksWhenThereAreNoMoves(soldier, field.hex(0, 0))
    assert(moves === Set(field.hex(0, 1)))
  }

  test("possible attacks when there are no moves should return enemies that are near when there are moves but soldier alread moved") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    soldier.movePointsRemain = 5
    field.hex(0, 0).soldier = Some(soldier)
    val enemy = new Soldier("2", simpleSoldierType, Player("2"))
    field.hex(0, 1).soldier = Some(enemy)
    val moves = model.possibleAttacksWhenThereAreNoMoves(soldier, field.hex(0, 0))
    assert(moves === Set(field.hex(0, 1)))
  }

  test("possible attacks when there are no moves should return empty set  when soldier haven't already moved even is there are enemies near") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    field.hex(0, 0).soldier = Some(soldier)
    val enemy = new Soldier("2", simpleSoldierType, Player("2"))
    field.hex(0, 1).soldier = Some(enemy)
    val moves = model.possibleAttacksWhenThereAreNoMoves(soldier, field.hex(0, 0))
    assert(moves === Set())
  }

  test("possible moves when enemy soldier is near") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    field.hex(0, 0).soldier = Some(soldier)
    val enemy = new Soldier("1", simpleSoldierType, Player("2"))
    field.hex(0, 1).soldier = Some(enemy)

    val possible = model.possibleMoves(soldier, field.hex(0, 0))
    assert(possible === Set(field.hex(1, 0)))
  }

  test("possible moves for player which move isn't now should think that he hasn't moved or attacked") {
    val simpleSoldierType = new SoldierType("1", 1, 20, 4, 5, 1,
      List(), Map(GrassKind -> 2),
      Map(), Map(), viewName = "")
    val soldier = new Soldier("1", simpleSoldierType, Player("2"))
    field.hex(0, 0).soldier = Some(soldier)
    val moves = model.possibleMoves(soldier, field.hex(0, 0))
    val currentField = field
    import currentField.hex
    assert(moves === Set(hex(0, 1), hex(1, 0), hex(2, 0), hex(2, 1), hex(1, 1), hex(0, 2)))

  }

  test("after battle soldier with 0 hp dissapear from field") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    field.hex(0, 0).soldier = Some(soldier)
    val enemy = new Soldier("1", simpleSoldierType, Player("2"))
    field.hex(0, 1).soldier = Some(enemy)

    while (enemy.hp != 0 && soldier.hp != 0) {
      soldier.attackedThisTurn = false
      model.handleAttackEvent(soldier, field.hex(0, 0), field.hex(0, 1), 0)
    }

    if (soldier.hp == 0) {
      assert(field.hex(0, 0).soldier === None)
    } else {
      assert(field.hex(0, 1).soldier === None)
    }
  }

  test("end turn and before turn events") {
    val curer = new SoldierType("1", 1, 20, 10, 5, 1,
      List(Attack(0, 10, 1, Impact, false), Attack(1, 6, 2, Impact, false)), Map(GrassKind -> 2),
      Map(GrassDefence -> 60), Map(Impact -> 0), Set(Cures, Heals4), viewName = "")
    val poisonedSoldier = new Soldier("1", curer, Player("1"))
    val enemy = new Soldier("1", simpleSoldierType, Player("2"))
    val damagedSoldier = new Soldier("2", curer, Player("1"))
    field.hex(0, 0).soldier = Some(damagedSoldier)
    field.hex(0, 1).soldier = Some(poisonedSoldier)
    field.hex(4, 4).soldier = Some(enemy)
    assert(model.currentPlayer === Player("1"))
    damagedSoldier.movePointsRemain -= 1
    poisonedSoldier.movePointsRemain -= 1
    model.handleEndTurnEvent()
    // during enemy's turn one of players is poisoned, another is damaged
    poisonedSoldier.addState(Poisoned)
    damagedSoldier.hp = 10

    model.handleEndTurnEvent()
    assert(poisonedSoldier.state === Set())
    assert(damagedSoldier.hp === 14)
  }

  test("not his turn is returned when it's not this soldier turn despite anything") {
    val soldier = new Soldier("1", simpleSoldierType, Player("2"))
    val hisEnemy = new Soldier("1", simpleSoldierType, Player("1"))
    field.hex(0, 0).soldier = Some(soldier)
    assert(model.soldierTurnState(field.hex(0, 0)) === NotHisTurn)
    field.hex(0, 1).soldier = Some(hisEnemy)
    assert(model.soldierTurnState(field.hex(0, 0)) === NotHisTurn)
    soldier.movePointsRemain -= 1
    assert(model.soldierTurnState(field.hex(0, 0)) === NotHisTurn)
    soldier.attackedThisTurn = true
    assert(model.soldierTurnState(field.hex(0, 0)) === NotHisTurn)
  }

  test("haven't moved turn state is returned when it's soldier turn and he haven't moved") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    field.hex(0, 0).soldier = Some(soldier)
    assert(model.soldierTurnState(field.hex(0, 0)) === HaventMoved)
  }

  test("cann't move is returned when soldier is near enemy and moved") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    val enemy = new Soldier("2", simpleSoldierType, Player("2"))
    field.hex(0, 0).soldier = Some(soldier)
    field.hex(0, 1).soldier = Some(enemy)
    assert(model.soldierTurnState(field.hex(0, 0)) === HaventMoved)
    soldier.movePointsRemain -= 1
    assert(model.soldierTurnState(field.hex(0, 0)) === CanntMoveAnyMore)
  }

  test("cann't move is returned when soldier cann't move any more") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    field.hex(0, 0).soldier = Some(soldier)
    soldier.movePointsRemain = 1
    assert(model.soldierTurnState(field.hex(0, 0)) === CanntMoveAnyMore)
  }

  test("attacked returned when soldier attacked") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    field.hex(0, 0).soldier = Some(soldier)
    soldier.attackedThisTurn = true
    assert(model.soldierTurnState(field.hex(0, 0)) === HaveAttacked)
  }

  test("still can't move return when soldier moved but have enough move points to continue moving") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    field.hex(0, 0).soldier = Some(soldier)
    soldier.movePointsRemain -= 1
    assert(model.soldierTurnState(field.hex(0, 0)) === StillCanMove)
  }

  test("road and wooden bridge terrain units are counted as grass") {
    def terrainHex(x: Int, y: Int) = if (x == 1 && y == 0) {
      new TerrainHex(x, y, OldRoad)
    } else if (x == 0 && y == 1) {
      new TerrainHex(x, y, ShallowWater, Some(WoodenBridge))
    } else {
      new TerrainHex(x, y, DesertSand)
    }

    field = new TerrainHexField(10, 10, terrainHex)
    model = new BattleModel(new GameField(field, List(Player("1"), Player("2")), Set(Set(Player("1")), Set(Player("2")))))
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    field.hex(0, 0).soldier = Some(soldier)
    soldier.movePointsRemain = 2
    val canMove1 = model.validateMovementEvent(soldier, field.hex(0, 0), field.hex(0, 1), true, true)
    assert(canMove1 === true)
    val canMove2 = model.validateMovementEvent(soldier, field.hex(0, 0), field.hex(1, 0), true, true)
    assert(canMove2 === true)
    soldier.movePointsRemain = 1
    val canntMove1 = model.validateMovementEvent(soldier, field.hex(0, 0), field.hex(0, 1), true, true)
    assert(canntMove1 === false)
    val canntMove2 = model.validateMovementEvent(soldier, field.hex(0, 0), field.hex(1, 0), true, true)
    assert(canntMove2 === false)
    soldier.movePointsRemain = 2
    model.handleMovementEvent(soldier, field.hex(0, 0), field.hex(1, 0))
    assert(field.hex(1, 0).soldier === Some(soldier))
    assert(soldier.movePointsRemain === 0)
  }

  test("village movement takes its price disregardly to terrain type") {
    def terrainHex(x: Int, y: Int) = if (x == 1 && y == 0) {
      new TerrainHex(x, y, OldRoad)
    } else if (x == 0 && y == 1) {
      new TerrainHex(x, y, ShallowWater, Some(HumanCityHouse))
    } else {
      new TerrainHex(x, y, DesertSand)
    }

    field = new TerrainHexField(10, 10, terrainHex)
    model = new BattleModel(new GameField(field, List(Player("1"), Player("2")), Set(Set(Player("1")), Set(Player("2")))))
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    field.hex(0, 0).soldier = Some(soldier)
    soldier.movePointsRemain = 3
    val canMove = model.validateMovementEvent(soldier, field.hex(0, 0), field.hex(0, 1), true, true)
    assert(canMove === true)
    soldier.movePointsRemain = 2
    val cantMove = model.validateMovementEvent(soldier, field.hex(0, 0), field.hex(0, 1), true, true)
    assert(cantMove === false)
    soldier.movePointsRemain = 3
    model.handleMovementEvent(soldier, field.hex(0, 0), field.hex(0, 1))
    assert(field.hex(0, 1).soldier === Some(soldier))
    assert(soldier.movePointsRemain === 0)
  }

  test("all soldiers") {
    val soldier = new Soldier("1", simpleSoldierType, Player("1"))
    assert(model.allSoldiers.isEmpty === true)
    field.hex(0, 0).soldier = Some(soldier)
    assert(model.allSoldiers.size === 1)
    assert(model.allSoldiers(0) === soldier)
    assert(model.allSoldiersWithHexes.size === 1)
    assert(model.allSoldiersWithHexes(0) === (soldier, field.hex(0, 0)))
  }
}

