package mr.merc.battle

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain.Grass
import mr.merc.unit.SoldierType
import mr.merc.unit.Impact
import mr.merc.unit.Attack
import mr.merc.unit.Soldier
import mr.merc.players.Player
import mr.merc.map.GameField
import mr.merc.battle.event.AttackModelEvent
import mr.merc.battle.event.AttackModelEventResult
import mr.merc.battle.event.MovementModelEvent
import mr.merc.battle.event.MovementModelEventResult

class BattleModelTest extends FunSuite with BeforeAndAfter {
	var field:TerrainHexField = _
	val simpleSoldierType = new SoldierType("1", 1, 20, 10, 5, 1, 
			List(Attack("", 10, 1, Impact, false), Attack("", 6, 2, Impact, false)), Map(Grass -> 2), 
			Map(Grass -> 60), Map(Impact -> 0))
	var model:BattleModel = _
	
	before {
	  field = new TerrainHexField(10, 10, (x, y) => new TerrainHex(x, y, Grass))
	  model = new BattleModel(new GameField(field, List(Player("1"), Player("2"))))
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
	
	test("defender weapon selection") {
	  val attackerSoldierType = new SoldierType("1", 1, 50, 10, 5, 1, 
			List(Attack("", 10, 1, Impact, false), Attack("", 20, 1, Impact, true)), 
			Map(Grass -> 2), Map(Grass -> 60), Map(Impact -> 0))
	  
	  val defenderSoldierTypeOnlyCloseCombat = new SoldierType("2", 1, 50, 10, 5, 1, 
			List(Attack("", 10, 1, Impact, false), Attack("", 6, 2, Impact, false), 
			    Attack("", 3, 3, Impact, false)), 
			Map(Grass -> 2), Map(Grass -> 60), Map(Impact -> 0))
	  
	  val defenderSoldierTypeOnlyOneAttack = new SoldierType("3", 1, 50, 10, 5, 1, 
			List(Attack("", 10, 1, Impact, true)), 
			Map(Grass -> 2), Map(Grass -> 60), Map(Impact -> 0))
	  
	  val attacker = new Soldier("1", attackerSoldierType, Player("1"))
	  val defenderCloseCombat = new Soldier("2", defenderSoldierTypeOnlyCloseCombat, Player("2"))
	  val defenderOnlyOneAttack = new Soldier("3", defenderSoldierTypeOnlyOneAttack, Player("3"))
	  val closeAttack = attackerSoldierType.attacks(0)
	  val rangedAttack = attackerSoldierType.attacks(1)
	  
	  val bestAttack = model.selectBestAttackForDefender(attacker, defenderCloseCombat, closeAttack)
	  assert(bestAttack.get === defenderCloseCombat.soldierType.attacks(1))
	  
	  val noAttack = model.selectBestAttackForDefender(attacker, defenderCloseCombat, rangedAttack)
	  assert(noAttack === None)
	  
	  val onlyAttack = model.selectBestAttackForDefender(attacker, defenderOnlyOneAttack, rangedAttack)
	  assert(onlyAttack.get === defenderOnlyOneAttack.soldierType.attacks(0))
	  
	  val noAttack2 = model.selectBestAttackForDefender(attacker, defenderOnlyOneAttack, closeAttack)
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
	      assert(result(0).attack === simpleSoldierType.attacks(0))
	      assert(result(1).attacker === defender)
	      assert(result(1).defender === attacker)
	      assert(result(1).attack === simpleSoldierType.attacks(1))
	      assert(result(2).attacker === defender)
	      assert(result(2).defender === attacker)
	      assert(result(2).attack === simpleSoldierType.attacks(1))
	      
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
			List(), Map(Grass -> 2), 
			Map(), Map())
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
}