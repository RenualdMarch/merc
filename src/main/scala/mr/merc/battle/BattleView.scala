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

// injecting soldier drawer for test purposes only
class BattleView(model:BattleModel, _soldierDrawer:SoldiersDrawer = new SoldiersDrawer) {
	val mapView = new MapView(model.map.hexField, _soldierDrawer)
	
	private val hexesViewMap = mapView.terrainView.hexes.map(v => (v.hex -> v)) toMap
	private val soldiersMap = mapView.soldiers.map(s => (s.soldier -> s)) toMap

	def update(time:Int) {
	  mapView.update(time)
	}
	
	def drawItself(gc:GraphicsContext) {
	  mapView.drawItself(gc)
	}
	
	def hexByPixel(x:Int, y:Int) = mapView.hexByPixel(x, y)
	
	def wrap(s:Soldier) = soldiersMap(s)
	
	def wrap(t:TerrainHex) = hexesViewMap(t)
	
	def handleEvent(event:BattleViewEvent) {
	  event match {
	    case AttackBattleViewEvent(attackerTerrainHex, defenderTerrainHex, result) => 
	      		handleAttackEvent(wrap(attackerTerrainHex), wrap(defenderTerrainHex), result)
	    case MoveBattleViewEvent(soldier, path) => handleMovementEvent(wrap(soldier), path map wrap)
	    case EndMoveViewEvent(nextPlayer) => handleEndMoveEvent(nextPlayer)
	    case ShowMovementOptions(hexes) => {
	      val options = hexes map wrap
	      mapView.terrainView.movementOptions = Some(options)
	    }
	    case HideMovementOptions => {
	      mapView.terrainView.movementOptions = None
	    }
	    case ShowArrow(src, dest) => {
	      mapView.terrainView.arrow = Some(wrap(src), wrap(dest))
	    }
	    case HideArrow => {
	      mapView.terrainView.arrow = None
	    }
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
	  mapView.addMovement(move)
	}
	
	private def handleMovementEvent(soldier:SoldierView, path:List[TerrainHexView]) {
	  val destination = path.tail
	  val departure = path.init
	  val allMovements = for ((a, b) <- departure zip destination) yield     
	    new SoldierMoveMovement(a, b, soldier)
	  mapView.addMovement(new MovementList(allMovements))
	}
	
	private def handleEndMoveEvent(player:Player) {
	  ???
	}
}