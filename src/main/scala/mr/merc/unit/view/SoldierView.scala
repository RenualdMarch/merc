package mr.merc.unit.view

import mr.merc.view.SpriteState
import mr.merc.view.Sprite
import mr.merc.image.MImage
import mr.merc.map.hex.Direction
import mr.merc.map.hex.HexField
import mr.merc.map.hex.Hex
import mr.merc.map.hex.TerrainHexView
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import mr.merc.view.move.Movement
import mr.merc.unit.Soldier
import mr.merc.view.move.SoldierViewMoveMovement
import mr.merc.view.move.MovementList

object SoldierView {  
  private [view] def coordsCorrection(dir:Direction):(Int, Int) = {
    val hexField = new TerrainHexField(4, 4, (x, y) => new TerrainHex(x, y, Grass))
    val center = hexField.hex(1, 1)
    val centerView = new TerrainHexView(center, hexField)
    val neig = hexField.neighboursWithDirections(1, 1)(dir)
    val neigView = new TerrainHexView(neig, hexField)
    
    (neigView.x - centerView.x, neigView.y - centerView.y)
  }
  
}

class SoldierView (soldier:Soldier) extends Sprite[SoldierViewState](SoldierTypeViewInfo(soldier.soldierType.name).images, StandState) {
  
  def moveMovement(path:List[TerrainHexView]):Movement = {
    val list = path zip path.tail map (p => new SoldierViewMoveMovement(p._1, p._2, this))
    new MovementList(list)
  }
  
  def attackMovement(current:TerrainHex, underAttack:TerrainHex):Movement = {
    ???
  }
}


