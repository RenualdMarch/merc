package mr.merc.map.view

import mr.merc.map.hex.view.TerrainHexFieldView
import mr.merc.map.hex.TerrainHexField
import scalafx.scene.canvas.GraphicsContext
import mr.merc.unit.view.SoldierView

// TODO replace all arguments with one object which represents model
class MapView(field:TerrainHexField) {
	val terrainView = new TerrainHexFieldView(field)
	val soldiersDrawer = new SoldiersDrawer()
	
	createSoldiers foreach (soldiersDrawer.addSoldier)
	
	private def createSoldiers:List[SoldierView] = {
	  val soldiers = terrainView.hexes.flatMap(h => {
		  val soldierOption = h.hex.soldier	  
	      soldierOption match {
	      	case Some(soldier) => {
	      		val view = new SoldierView(soldier)
	      		view.x = h.x
	      		view.y = h.y
	      		Some(view)
	        }
	       case None => None
	      } 
	   
	  })
	  
	  soldiers toList
	} 
	
	def update(time:Int) {
	  soldiersDrawer.update(time)
	}
	
	def drawItself(gc:GraphicsContext) {
	  terrainView.drawItself(gc)
	  soldiersDrawer.drawSoldiers(gc)
	} 
}