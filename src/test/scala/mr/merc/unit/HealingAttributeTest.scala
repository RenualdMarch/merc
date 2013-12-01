package mr.merc.unit

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.terrain.Sand
import mr.merc.battle.BattleModel
import mr.merc.map.hex.TerrainHex
import mr.merc.map.GameField
import mr.merc.players.Player

class HealingAttributeTest extends FunSuite with BeforeAndAfter {
	var field:TerrainHexField = _
	def simpleSoldierType(attributes:Set[SoldierTypeAttribute] = Set()) = new SoldierType("1", 1, 20, 10, 5, 1, 
			List(Attack("", 10, 1, Impact, false), Attack("", 6, 2, Impact, false)), Map(Sand -> 2), 
			Map(Sand -> 60), Map(Impact -> 0), attributes)
	
	before {
	  field = new TerrainHexField(10, 10, (x, y) => new TerrainHex(x, y, Sand))
	}
  
    test("healing4 before action") {
	  val weakendSoldier = new Soldier("1", simpleSoldierType(), Player("1"))
	  weakendSoldier.hp = 10
	  val weakendSoldier2 = new Soldier("2", simpleSoldierType(), Player("1"))
	  weakendSoldier2.hp = 10
	  val healer = new Soldier("3", simpleSoldierType(Set(Heals4)), Player("1"))
	  field.hex(0, 1).soldier = Some(weakendSoldier)
	  field.hex(1, 0).soldier = Some(healer)
	  field.hex(5, 5).soldier = Some(weakendSoldier2)
	  val list = healer.beforeTurnActions(field, 1, 0)
	  assert(list.size === 1)
	  list(0) match {
	    case Heal4Soldier(currentHealer, underHeal) => {
	      assert(currentHealer === healer)
	      assert(underHeal === weakendSoldier)
	    }
	    case _ => fail
	  }
	}
    
    test("healing4 action is not called if ther is no need") {
      val weakendSoldier = new Soldier("1", simpleSoldierType(), Player("1"))
	  val healer = new Soldier("3", simpleSoldierType(Set(Heals4)), Player("1"))
	  field.hex(0, 1).soldier = Some(weakendSoldier)
	  field.hex(1, 0).soldier = Some(healer)
	  val list = healer.beforeTurnActions(field, 1, 0)
	  assert(list.size === 0)      
    }
	
	test("healing8 before action") {
	  val weakendSoldier = new Soldier("1", simpleSoldierType(), Player("1"))
	  weakendSoldier.hp = 10
	  val weakendSoldier2 = new Soldier("2", simpleSoldierType(), Player("1"))
	  weakendSoldier2.hp = 10
	  val healer = new Soldier("3", simpleSoldierType(Set(Heals8)), Player("1"))
	  field.hex(0, 1).soldier = Some(weakendSoldier)
	  field.hex(1, 0).soldier = Some(healer)
	  field.hex(5, 5).soldier = Some(weakendSoldier2)
	  val list = healer.beforeTurnActions(field, 1, 0)
	  assert(list.size === 1)
	  list(0) match {
	    case Heal8Soldier(currentHealer, underHeal) => {
	      assert(currentHealer === healer)
	      assert(underHeal === weakendSoldier)
	    }
	    case _ => fail
	  }	  
	}
	
	test("healing8 action is not called if ther is no need") {
      val weakendSoldier = new Soldier("1", simpleSoldierType(), Player("1"))
	  val healer = new Soldier("3", simpleSoldierType(Set(Heals8)), Player("1"))
	  field.hex(0, 1).soldier = Some(weakendSoldier)
	  field.hex(1, 0).soldier = Some(healer)
	  val list = healer.beforeTurnActions(field, 1, 0)
	  assert(list.size === 0)      
    }
	
	test("healing4 and curing together") {
      val weakendSoldier = new Soldier("1", simpleSoldierType(), Player("1"))
	  weakendSoldier.hp = 10
	  weakendSoldier.addState(Poisoned)
	  val healer = new Soldier("3", simpleSoldierType(Set(Heals4, Cures)), Player("1"))
	  field.hex(0, 1).soldier = Some(weakendSoldier)
	  field.hex(1, 0).soldier = Some(healer)
	  val list = healer.beforeTurnActions(field, 1, 0)
	  assert(list.size === 2)
	  val healing = list.find(_.isInstanceOf[Heal4Soldier]).get
	  healing match {
	    case Heal4Soldier(currentHealer, underHeal) => {
	      assert(currentHealer === healer)
	      assert(underHeal === weakendSoldier)
	    }
	    case _ => fail
	  }
      
      val curing = list.find(_.isInstanceOf[CureSoldier]).get
      curing match {
	    case CureSoldier(currentHealer, underHeal) => {
	      assert(currentHealer === healer)
	      assert(underHeal === weakendSoldier)
	    }
	    case _ => fail
	  }
	}
	
	test("healing8 and curing together") {
	  val weakendSoldier = new Soldier("1", simpleSoldierType(), Player("1"))
	  weakendSoldier.hp = 10
	  weakendSoldier.addState(Poisoned)
	  val healer = new Soldier("3", simpleSoldierType(Set(Heals8, Cures)), Player("1"))
	  field.hex(0, 1).soldier = Some(weakendSoldier)
	  field.hex(1, 0).soldier = Some(healer)
	  val list = healer.beforeTurnActions(field, 1, 0)
	  assert(list.size === 2)
	  val healing = list.find(_.isInstanceOf[Heal8Soldier]).get
	  healing match {
	    case Heal8Soldier(currentHealer, underHeal) => {
	      assert(currentHealer === healer)
	      assert(underHeal === weakendSoldier)
	    }
	    case _ => fail
	  }
      
      val curing = list.find(_.isInstanceOf[CureSoldier]).get
      curing match {
	    case CureSoldier(currentHealer, underHeal) => {
	      assert(currentHealer === healer)
	      assert(underHeal === weakendSoldier)
	    }
	    case _ => fail
	  }
	}
	
	test("healing4") {
	  val weakendSoldier = new Soldier("1", simpleSoldierType(), Player("1"))
	  weakendSoldier.hp = 10
	  weakendSoldier.movePointsRemain -= 1
	  Heal4Soldier(null, weakendSoldier).action()
	  assert(weakendSoldier.hp === 14)
	}
	
	test("healing4 when soldier didn't move") {
	  val weakendSoldier = new Soldier("1", simpleSoldierType(), Player("1"))
	  weakendSoldier.hp = 10
	  Heal4Soldier(null, weakendSoldier).action()
	  assert(weakendSoldier.hp === 16)
	}
	
	test("healing8") {
	  val weakendSoldier = new Soldier("1", simpleSoldierType(), Player("1"))
	  weakendSoldier.hp = 10
	  weakendSoldier.movePointsRemain -= 1
	  Heal8Soldier(null, weakendSoldier).action()
	  assert(weakendSoldier.hp === 18)	  
	}
	
	test("healing8 when soldier didn't move") {
	  val weakendSoldier = new Soldier("1", simpleSoldierType(), Player("1"))
	  weakendSoldier.hp = 10
	  Heal8Soldier(null, weakendSoldier).action()
	  assert(weakendSoldier.hp === 20)	  
	}
}