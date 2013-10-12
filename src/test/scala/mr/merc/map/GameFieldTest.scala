package mr.merc.map

import org.scalatest.FunSuite
import mr.merc.players.Player
import mr.merc.unit.Soldier
import mr.merc.unit.SoldierType
import mr.merc.map.hex._
import mr.merc.map.terrain._

class GameFieldTest extends FunSuite {
    val soldierType = new SoldierType("someType", 10, 10, 6, 10, 1, 
    List(), Map((Grass-> 2)), Map((Grass-> 50)), 
    Map())
  
	val player1 = Player("1")
	val player2 = Player("2")
	
	val soldier1 = new Soldier("1", soldierType, player1)
	val soldier2 = new Soldier("2", soldierType, player2)
	val soldier3 = new Soldier("2", soldierType, player2)
    
	val terrainField = new TerrainHexField(5, 5, (x, y) => new TerrainHex(x, y, Grass))
	val gameField = new GameField(terrainField, List(player1, player2))
	
	terrainField.hex(0, 1).soldier = Some(soldier1)
	terrainField.hex(1, 2).soldier = Some(soldier2)
	terrainField.hex(3, 2).soldier = Some(soldier3)
	import terrainField._
	
	test("zone of control") {      
      val zone1 = gameField.zoneOfControlFor(player1)
      assert(zone1 === Set(hex(0, 0), hex(0, 1), hex(0, 2), hex(1, 0), hex(1, 1)))
      
      val zone2 = gameField.zoneOfControlFor(player2)
      assert(zone2 === Set(hex(0, 2), hex(0, 3), hex(1, 1), hex(1, 2), hex(1, 3),
          hex(2, 2), hex(2, 3), hex(3, 1), hex(3, 2), hex(3, 3), hex(4, 2), hex(4, 3)))
    }
    
    test("grid retrieval") {
      import terrainField._
      val grid1 = gameField.gridForSoldier(soldier1)
      assert(grid1.isBlocked(hex(0, 1)) === false)
      assert(grid1.isBlocked(hex(0, 2)) === false)
      assert(grid1.isBlocked(hex(0, 3)) === false)
      assert(grid1.isBlocked(hex(1, 2)) === true)
      assert(grid1.isBlocked(hex(3, 2)) === true)
      
      assert(grid1.price(hex(0, 3)) === 2)
      
      assert(grid1.cellWhereItIsForbiddenToStop(hex(0, 1)) === false)
      assert(grid1.cellWhereItIsForbiddenToStop(hex(1, 2)) === true)
      assert(grid1.cellWhereItIsForbiddenToStop(hex(3, 2)) === true)
      assert(grid1.cellWhereItIsForbiddenToStop(hex(0, 2)) === false)
      assert(grid1.cellWhereItIsForbiddenToStop(hex(2, 2)) === false)
      assert(grid1.cellWhereItIsForbiddenToStop(hex(3, 3)) === false)
      
      assert(grid1.cellWhereMovementMustBeStopped(hex(0, 1)) === false)
      assert(grid1.cellWhereMovementMustBeStopped(hex(0, 0)) === false)
      assert(grid1.cellWhereMovementMustBeStopped(hex(3, 1)) === true)
      assert(grid1.cellWhereMovementMustBeStopped(hex(1, 2)) === true)
      
      val grid2 = gameField.gridForSoldier(soldier2)
      assert(grid2.isBlocked(hex(3, 2)) === false)
      assert(grid2.isBlocked(hex(0, 1)) === true)
      assert(grid2.cellWhereItIsForbiddenToStop(hex(0, 1)) === true)
      assert(grid2.cellWhereItIsForbiddenToStop(hex(1, 2)) === false)
      assert(grid2.cellWhereItIsForbiddenToStop(hex(3, 2)) === true)
      assert(grid2.cellWhereItIsForbiddenToStop(hex(0, 2)) === false)
      assert(grid2.cellWhereItIsForbiddenToStop(hex(2, 2)) === false)
      assert(grid2.cellWhereItIsForbiddenToStop(hex(3, 3)) === false)
      
      assert(grid2.cellWhereMovementMustBeStopped(hex(0, 1)) === true)
      assert(grid2.cellWhereMovementMustBeStopped(hex(0, 0)) === true)
      assert(grid2.cellWhereMovementMustBeStopped(hex(3, 1)) === false)
      assert(grid2.cellWhereMovementMustBeStopped(hex(1, 2)) === false)
      
    }
}