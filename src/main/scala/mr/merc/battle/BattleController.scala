package mr.merc.battle


import scalafx.scene.canvas.Canvas
import mr.merc.map.hex.TerrainHexField
import mr.merc.unit.Soldier
import mr.merc.map.view.MapView
import mr.merc.players.Player
import mr.merc.unit.SoldierType
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import mr.merc.map.objects._
import scalafx.scene.canvas.GraphicsContext
import mr.merc.map.GameField
import mr.merc.ui.common.SoldierWrapper
import mr.merc.battle.event.ShowArrow
import mr.merc.battle.event.HideArrow
import mr.merc.battle.event.HideMovementOptions
import mr.merc.battle.event.ShowMovementOptions
import mr.merc.battle.event.MovementModelEvent
import mr.merc.battle.event.AttackModelEvent
import mr.merc.ui.battle.BattleFrame


class BattleController(gameField:GameField, parent:BattleControllerParent)  {
  val battleModel = new BattleModel(gameField)
  val battleView = new BattleView(battleModel)
  
  val soldierToShow = new SoldierWrapper(None)
  
  private [battle] var selectedSoldier:Option[Soldier] = None
  private [battle] var arrowIsShown = false
  private [battle] var movementOptionsAreShown = false
  private [battle] val visitedHexesList = new VisitedHexesList()
  
  def moveMouse(x:Int, y:Int) {
    val hexOpt = battleView.hexByPixel(x, y)
    hexOpt match {
      case Some(hexView) => {
        visitedHexesList.visitHex(hexView.hex)
        hexView.hex.soldier match {
          case Some(soldier) => {
            soldierToShow.soldier = Some(soldier)
            if(canBeCurrentHexAttackedFromPrevious) {
              drawArrowForPrevToCurrent()
            } else {
              removeArrow()
            }
          }
          case None => {
            removeArrow()
            soldierToShow.soldier = selectedSoldier
          }
        }
      }
      case None => {
        removeArrow()
      }
    }
  }
  
  private def removeArrow() {
    if (arrowIsShown) {
      arrowIsShown = false
      battleView.handleEvent(HideArrow)
    }
  }
  
  private def removeMovementOptions() {
    if (movementOptionsAreShown) {
      movementOptionsAreShown = false
      battleView.handleEvent(HideMovementOptions)
    }
  }
  
  private def addMovementOptionsForSelectedSoldier() {
    val hexOpt = battleModel.hexBySoldier(selectedSoldier)
    hexOpt match {
      case Some(hex) => {
        movementOptionsAreShown = true
        val possible = battleModel.possibleMoves(selectedSoldier.get, hex) ++ Set(hex) ++
          battleModel.possibleAttacksWhenThereAreNoMoves(selectedSoldier.get, hex)
          battleView.handleEvent(ShowMovementOptions(possible))
      }
      case None => // do nothing
    }
  }
  
  def rightClickMouse() = withCurrent(hex => {
    hex.soldier match {
      case Some(soldier) => {
    	selectedSoldier match {
    	  case Some(attacker) => {
    	    val attackerHex = battleModel.hexBySoldier(attacker)
    	    
    	    // TODO think we should remove selected soldier
            selectedSoldier = None   
    	    
            removeArrow()
            removeMovementOptions()
            
             if (visitedHexesList.isDefined && battleModel.validateMovementAndAttack(attacker, attackerHex, 
        		visitedHexesList.prev.get, visitedHexesList.current.get, 0)) {
               
               if (attackerHex != visitedHexesList.prev.get) {
                 val moveResult = battleModel.handleMovementEvent(attacker, attackerHex, visitedHexesList.prev.get)
                 battleView.handleEvent(moveResult.buildBattleViewEvent)
               }
               
               // TODO IMPORTANT!!! Here add attack selection via dialog!
               val attackResult = battleModel.handleEvent(AttackModelEvent(attacker, visitedHexesList.prev.get, hex, 0))
               battleView.handleEvent(attackResult.buildBattleViewEvent)
            }        
            
    	  }
    	  case None => // TODO do something, do nothing by now
    	}
      }
      case None => {
        selectedSoldier match {
          case Some(soldier) => {
            val from = battleModel.hexBySoldier(soldier)
            if (battleModel.validateMovementEvent(soldier, from, hex)) {
              val result = battleModel.handleEvent(MovementModelEvent(soldier, from, hex))
              battleView.handleEvent(result.buildBattleViewEvent)
            }
            
            selectedSoldier = None
            soldierToShow.soldier = None
            removeMovementOptions()
          }
          case None => // do nothing
        }
      }
    }
  })
  
  def leftClickMouse() = withCurrent(hex => {
    hex.soldier match {
      case Some(soldier) => {
        selectedSoldier = Some(soldier)
        // TODO add if soldier player is human player
        addMovementOptionsForSelectedSoldier()
        removeArrow()
      }
      case None => {
        selectedSoldier = None
        removeArrow()
        removeMovementOptions()
      }
    }
    soldierToShow.soldier = selectedSoldier

  })
  
  private def withCurrent[T](f:TerrainHex => T) {
    val hexOpt = visitedHexesList.current
    hexOpt match {
      case Some(hex) => {
        f(hex)
      }
      case None => // do nothing
    }
  }
  
  private [battle] def canBeCurrentHexAttackedFromPrevious:Boolean = {
    val canSoldierAttack = selectedSoldier.map(!_.attackedThisTurn).getOrElse(false)
    if (!canSoldierAttack) {
      false
    } else {
      val hexOpt = battleModel.hexBySoldier(selectedSoldier)
      if (visitedHexesList.isDefined && hexOpt.isDefined) {
        battleModel.validateMovementAndAttack(selectedSoldier.get, hexOpt.get, visitedHexesList.prev.get, visitedHexesList.current.get, 0)
      } else {
        false
      }
    }
  }
  
  private def drawArrowForPrevToCurrent() {
     if (visitedHexesList.isDefined) {
       arrowIsShown = true
       battleView.handleEvent(ShowArrow(visitedHexesList.prev.get, 
           visitedHexesList.current.get))
     }
  }
  
  def update(time:Int) {
    battleView.update(time)
  }
  
  def drawBattleCanvas(gc:GraphicsContext) {
    battleView.drawItself(gc)
  }
}

private [battle] class VisitedHexesList {
  private var _prev:Option[TerrainHex] = None
  private var _current:Option[TerrainHex] = None
  def prev = _prev
  def current = _current
  def clean() {
    _prev = None
    _current = None
  }
  def visitHex(hex:TerrainHex) {
    if (_current != Some(hex)) {
      _prev = _current
      _current = Some(hex)
    }
  }
  def isDefined = prev.isDefined && current.isDefined
}