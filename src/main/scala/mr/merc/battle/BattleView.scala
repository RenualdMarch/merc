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

class BattleView(model:BattleModel) {
	val mapView = new MapView(model.map.hexField)
	
	val soldierDrawer = new SoldiersDrawer
	model.map.hexField.hexes.foreach(_.soldier.foreach(s => soldierDrawer.addSoldier(new SoldierView(s))))
	
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
}