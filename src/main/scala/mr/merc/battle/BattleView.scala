package mr.merc.battle

import mr.merc.map.view.MapView
import mr.merc.map.GameField
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.view.SoldiersDrawer
import mr.merc.unit.view.SoldierView
import scalafx.scene.canvas.GraphicsContext
import mr.merc.unit.Soldier
import mr.merc.map.hex.TerrainHex
import mr.merc.map.hex.view.TerrainHexView
import mr.merc.battle.event._
import mr.merc.unit.AttackResult
import mr.merc.view.move.SoldierMoveMovement
import mr.merc.view.move.MovementList
import mr.merc.players.Player
import mr.merc.unit.AttackResult
import mr.merc.view.move.SoldierAttackMovement
import mr.merc.view.move.SoldierRangedAttackMovement

class BattleView(model:BattleModel, private val soldierDrawer:SoldiersDrawer = new SoldiersDrawer) {
	val mapView = new MapView(model.map.hexField)
		
	model.map.hexField.hexes foreach(h => h.soldier foreach(s => {
	  val view = new SoldierView(s)
	  view.x = wrap(h).x
	  view.y = wrap(h).y
	  soldierDrawer.addSoldier(view)
    }))
	
	def update(time:Int) {
	  mapView.update(time)
	  soldierDrawer.update(time)
	}
	
	def drawItself(gc:GraphicsContext) {
	  mapView.drawItself(gc)
	  soldierDrawer.drawSoldiers(gc)
	}
	
	// TODO this operation can be speed up from n to c
	def wrap(s:Soldier):SoldierView = {
	  soldierDrawer.soldiers.find(_.soldier == s).get
	}
	
	// TODO this operation can be speed up from n to c
	def wrap(t:TerrainHex):TerrainHexView = {
	  mapView.terrainView.hexes.find(_.hex == t).get
	}
	
	def handleEvent(event:BattleViewEvent) {
	  event match {
	    case AttackBattleViewEvent(attackerTerrainHex, defenderTerrainHex, result) => 
	      		handleAttackEvent(wrap(attackerTerrainHex), wrap(defenderTerrainHex), result)
	    case MoveBattleViewEvent(soldier, path) => handleMovementEvent(wrap(soldier), path map wrap)
	    case EndMoveViewEvent(nextPlayer) => handleEndMoveEvent(nextPlayer)
	  }
	}
	
	private def handleAttackEvent(attackerTerrainHex:TerrainHexView, 
				defenterTerrainHex:TerrainHexView, result:List[AttackResult]) {	  
	  val dir = model.map.hexField.direction(attackerTerrainHex.hex, defenterTerrainHex.hex)
	  	  
	  def factory(res:AttackResult) = {
	    val attacker = res.attacker
	    val defender = res.defender
	    val attack = res.attack
	    val ranged = attack.ranged
	    val attackNumber = res.attacker.soldierType.attacks.indexOf(attack)
	    if(ranged) {
	      new SoldierRangedAttackMovement(attackerTerrainHex.coords, defenterTerrainHex.coords, dir,
	    			res.success, wrap(attacker), wrap(defender), attackNumber)
	    } else {
	      new SoldierAttackMovement(attackerTerrainHex.coords, defenterTerrainHex.coords, dir,
	    			res.success, wrap(attacker), wrap(defender), attackNumber)
	    }
	  }
	  
	  val move = new MovementList(result map factory)
	  soldierDrawer.addMovement(move)
	}
	
	private def handleMovementEvent(soldier:SoldierView, path:List[TerrainHexView]) {
	  val destination = path.tail
	  val departure = path.init
	  val allMovements = for ((a, b) <- departure zip destination) yield     
	    new SoldierMoveMovement(a, b, soldier)
	  soldierDrawer.addMovement(new MovementList(allMovements))
	}
	
	private def handleEndMoveEvent(player:Player) {
	  ???
	}
}