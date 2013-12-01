package mr.merc.unit

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.terrain.Sand
import mr.merc.battle.BattleModel
import mr.merc.map.hex.TerrainHex
import mr.merc.map.GameField
import mr.merc.players.Player

class RegeneratesAttributeTest extends FunSuite with BeforeAndAfter {
    var field:TerrainHexField = _
	def simpleSoldierType(attributes:Set[SoldierTypeAttribute] = Set()) = new SoldierType("1", 1, 20, 10, 5, 1, 
			List(Attack("", 10, 1, Impact, false), Attack("", 6, 2, Impact, false)), Map(Sand -> 2), 
			Map(Sand -> 60), Map(Impact -> 0), attributes)
	var soldier:Soldier = _
    
	before {
	  field = new TerrainHexField(10, 10, (x, y) => new TerrainHex(x, y, Sand))
	  soldier = new Soldier("1", simpleSoldierType(Set(Regenerates)), Player("1"))
	}
  
	test("regenerates removes poison and don't change health") {
	  soldier.hp = 10
	  soldier.addState(Poisoned)
	  Regeneration(soldier).action()
	  assert(soldier.state === Set())
	  assert(soldier.hp === 10)
	}
	
	test("regenerates improves health when soldier is not poisoned") {
	  soldier.hp = 10
	  soldier.movePointsRemain -= 1
	  Regeneration(soldier).action()
	  assert(soldier.state === Set())
	  assert(soldier.hp === 18)
	}
	
	test("regenerates improves more health when soldier didn't move") {
	  soldier.hp = 10
	  Regeneration(soldier).action()
	  assert(soldier.state === Set())
	  assert(soldier.hp === 20)
	}
	
	
	test("regenerates is not created when unit is ok") {
	  val actions = soldier.beforeTurnActions(field, 1, 1)
	  assert(actions.size === 0)
	}
	
	test("regenerates is created when unit is poisoned") {
	  soldier.addState(Poisoned)
	  val actions = soldier.beforeTurnActions(field, 1, 1)
	  assert(actions.size === 2)
	  actions.find(_.isInstanceOf[Regeneration]).get match {
	    case Regeneration(someone) => assert(someone === soldier)
	    case _ => fail
	  }
	  actions.find(_.isInstanceOf[PoisoningDamage]).get
	}
	
	test("regenerates is created when units hp is not max") {
	  soldier.hp -= 1
	  val actions = soldier.beforeTurnActions(field, 1, 1)
	  assert(actions.size === 1)
	  actions(0) match {
	    case Regeneration(someone) => assert(someone === soldier)
	    case _ => fail
	  }
	}
	
	test("regenerates is created when unit is poisoned and hp is not max") {
	  soldier.hp -= 1
	  soldier.addState(Poisoned)
	  val actions = soldier.beforeTurnActions(field, 1, 1)
	  assert(actions.size === 2)
	  actions.find(_.isInstanceOf[Regeneration]).get match {
	    case Regeneration(someone) => assert(someone === soldier)
	    case _ => fail
	  }
	  actions.find(_.isInstanceOf[PoisoningDamage]).get
	}	
}