package mr.merc.unit.view

import mr.merc.view.SpriteState
import mr.merc.view.Sprite
import mr.merc.image.MImage
import mr.merc.map.hex.Direction
import mr.merc.map.hex.HexField
import mr.merc.map.hex.Hex
import mr.merc.map.hex.view.TerrainHexView
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import mr.merc.view.move.Movement
import mr.merc.unit.Soldier
import mr.merc.view.move.SoldierViewMoveMovement
import mr.merc.view.move.MovementList
import mr.merc.view.move.SoldierAttackMovement
import mr.merc.unit.AttackResult

object SoldierView {  
  private [view] val attackDistancePercentage = 0.6
  
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
  
  def attackMovement(current:TerrainHexView, underAttack:TerrainHexView, enemy:SoldierView, result:AttackResult):Movement = {
    val attackNumber = soldier.soldierType.attacks.indexOf(result.attack)
    val direction = current.directions(underAttack.hex)
    require(attackNumber == 0 || attackNumber == 1, s"attack number is illegal: $attackNumber")
    val from = (current.x, current.y)
    val correction = SoldierView.coordsCorrection(direction)
    val actualCorrection = (correction._1 * SoldierView.attackDistancePercentage toInt,
    						correction._2 * SoldierView.attackDistancePercentage toInt)
    val to = (from._1 + actualCorrection._1, from._2 + actualCorrection._2)
    new SoldierAttackMovement(from, to, direction, result.success, this, enemy, attackNumber)
  }
}


