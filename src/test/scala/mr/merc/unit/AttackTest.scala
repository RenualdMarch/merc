package mr.merc.unit

import org.scalatest.FunSuite
import mr.merc.map.terrain.TerrainType
import mr.merc.map.terrain.Sand
import org.scalatest.BeforeAndAfter
import mr.merc.map.hex.TerrainHex
import mr.merc.players.Player

class AttackTest extends FunSuite with BeforeAndAfter {
	val player1 = Player("1")
	val player2 = Player("2")
    val firstType = new SoldierType("type1", 1, 10, 1, 10, 1, 
			List(new Attack("name", 3, 2, Impact, false), new Attack("name", 1, 2, Pierce, true)), 
			Map(), Map(Sand -> 50), Map(Impact -> 0, Pierce -> 0))
  
	val secondType = new SoldierType("type2", 1, 12, 1, 10, 1, 
			List(new Attack("name", 2, 3, Impact, false)), 
			Map(), Map(Sand -> 70), Map(Impact -> 0, Pierce -> 0))
  
	var firstSoldier:Soldier = _
	var secondSoldier:Soldier = _
	val attackerHex = new TerrainHex(0, 0, Sand)
	val defenderHex = new TerrainHex(0, 1, Sand)
    
    def allMisses(x:Int) = false
    def firstHitsSecondMisses(x:Int) = if (x == 70) true 
    	else if (x == 50) false else fail
    
	before {
      firstSoldier = new Soldier("first", firstType, player1)
      secondSoldier = new Soldier("second", secondType, player2)
      attackerHex.soldier = Some(firstSoldier)
      defenderHex.soldier = Some(secondSoldier)
    }
	
    test("attack and defence both strike") {
	  val results = Attack.battle(attackerHex, defenderHex, firstType.attacks(0), Some(secondType.attacks(0)), allMisses)
	  assert(results.size === 5)
	  assert(results(0).attacker === firstSoldier)
	  assert(results(0).defender === secondSoldier)
	  assert(results(0).attack === firstType.attacks(0))
	  assert(results(0).success === false)
	  
	  assert(results(1).attacker === secondSoldier)
	  assert(results(1).defender === firstSoldier)
	  assert(results(1).attack === secondType.attacks(0))
	  assert(results(1).success === false)
    
	  assert(results(2).attacker === firstSoldier)
	  assert(results(2).defender === secondSoldier)
	  assert(results(2).attack === firstType.attacks(0))
	  assert(results(2).success === false)
	  
	  assert(results(3).attacker === secondSoldier)
	  assert(results(3).defender === firstSoldier)
	  assert(results(3).attack === secondType.attacks(0))
	  assert(results(3).success === false)
	  
	  assert(results(4).attacker === secondSoldier)
	  assert(results(4).defender === firstSoldier)
	  assert(results(4).attack === secondType.attacks(0))
	  assert(results(4).success === false)
    }
	
	test("only attack strikes") {
	  val results = Attack.battle(attackerHex, defenderHex, firstType.attacks(1), None, allMisses)
	  assert(results.size === 2)
	  assert(results(0).attacker === firstSoldier)
	  assert(results(0).defender === secondSoldier)
	  assert(results(0).attack === firstType.attacks(1))
	  assert(results(0).success === false)
	  
	  assert(results(1).attacker === firstSoldier)
	  assert(results(1).defender === secondSoldier)
	  assert(results(1).attack === firstType.attacks(1))
	  assert(results(1).success === false)
	}
	
	test("attacks and death") {
	  val firstResults = Attack.battle(attackerHex, defenderHex, firstType.attacks(0), Some(secondType.attacks(0)), firstHitsSecondMisses)
	  
	  assert(firstResults.size === 5)
	  assert(firstResults(0).attacker === firstSoldier)
	  assert(firstResults(0).defender === secondSoldier)
	  assert(firstResults(0).attack === firstType.attacks(0))
	  assert(firstResults(0).success === true)
	  
	  assert(firstResults(1).attacker === secondSoldier)
	  assert(firstResults(1).defender === firstSoldier)
	  assert(firstResults(1).attack === secondType.attacks(0))
	  assert(firstResults(1).success === false)
    
	  assert(firstResults(2).attacker === firstSoldier)
	  assert(firstResults(2).defender === secondSoldier)
	  assert(firstResults(2).attack === firstType.attacks(0))
	  assert(firstResults(2).success === true)
	  
	  assert(firstResults(3).attacker === secondSoldier)
	  assert(firstResults(3).defender === firstSoldier)
	  assert(firstResults(3).attack === secondType.attacks(0))
	  assert(firstResults(3).success === false)
	  
	  assert(firstResults(4).attacker === secondSoldier)
	  assert(firstResults(4).defender === firstSoldier)
	  assert(firstResults(4).attack === secondType.attacks(0))
	  assert(firstResults(4).success === false)
	  
	  val secondResults = Attack.battle(attackerHex, defenderHex, firstType.attacks(0), Some(secondType.attacks(0)), firstHitsSecondMisses)
	  
	  assert(secondResults.size === 3)
	  assert(secondResults(0).attacker === firstSoldier)
	  assert(secondResults(0).defender === secondSoldier)
	  assert(secondResults(0).attack === firstType.attacks(0))
	  assert(secondResults(0).success === true)
	  
	  assert(secondResults(1).attacker === secondSoldier)
	  assert(secondResults(1).defender === firstSoldier)
	  assert(secondResults(1).attack === secondType.attacks(0))
	  assert(secondResults(1).success === false)
    
	  assert(secondResults(2).attacker === firstSoldier)
	  assert(secondResults(2).defender === secondSoldier)
	  assert(secondResults(2).attack === firstType.attacks(0))
	  assert(secondResults(2).success === true)
	  
	  assert(secondSoldier.hp === 0)
	  assert(firstSoldier.hp === 10)
	}
}