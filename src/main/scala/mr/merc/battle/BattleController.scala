package mr.merc.battle

import mr.merc.ai.BattleAI
import mr.merc.unit.Soldier
import mr.merc.players.Player
import mr.merc.map.hex.TerrainHex
import mr.merc.map.GameField
import mr.merc.ui.common.SoldierWrapper
import mr.merc.battle.event.ShowArrow
import mr.merc.battle.event.HideArrow
import mr.merc.battle.event.HideMovementOptions
import mr.merc.battle.event.ShowMovementOptions
import mr.merc.battle.event.MovementModelEvent
import mr.merc.battle.event.AttackModelEvent
import mr.merc.battle.event.EndMoveModelEvent
import mr.merc.unit.Attack
import mr.merc.battle.event.HideDefence
import mr.merc.battle.event.ShowDefence
import mr.merc.log.Logging
import scalafx.geometry.Rectangle2D

class BattleController(gameField: GameField, parent: BattleControllerParent, factor: Double, aiMap:Map[Player, BattleAI]) extends Logging {
  val battleModel = new BattleModel(gameField)
  val battleView = new BattleView(battleModel, factor)

  val soldierToShow = new SoldierWrapper(None, factor)

  private[battle] var selectedSoldier: Option[Soldier] = None
  private[battle] var arrowIsShown = false
  private[battle] var defenceIsShown = false
  private[battle] var movementOptionsAreShown = false
  private[battle] val visitedHexesList = new VisitedHexesList()

  def moveMouse(x: Int, y: Int, viewRect: Rectangle2D) {
    debug(s"Mouse moved to ($x, $y)")
    val hexOpt = battleView.hexByPixel(x, y, viewRect)
    hexOpt match {
      case Some(hexView) =>
        visitedHexesList.visitHex(hexView.hex)
        hexView.hex.soldier match {
          case Some(soldier) =>
            removeDefence()
            soldierToShow.soldier = Some(soldier)
            if (canBeCurrentHexAttackedFromPrevious) {
              drawArrowForPrevToCurrent()
              drawDefenceForHexFromWhichAttack()
            } else if (onlyChoiceForSelectedSoldierIsAttack) {
              drawArrowWhenOnlyChoiceForSelectedSoldierIsAttack()
            } else {
              removeArrow()
            }

          case None =>
            removeArrow()
            drawDefenceForSelectedTerrainHex()
            soldierToShow.soldier = selectedSoldier

        }

      case None =>
        removeArrow()
        removeDefence()

    }
  }

  def mouseLeftCanvas() {
    removeArrow()
    removeDefence()
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

  private def removeDefence() {
    if (defenceIsShown) {
      defenceIsShown = false
      battleView.handleEvent(HideDefence)
    }
  }

  private def addMovementOptionsForSelectedSoldier() {
    val hexOpt = battleModel.hexBySoldier(selectedSoldier)
    hexOpt match {
      case Some(hex) =>
        movementOptionsAreShown = true
        val possible = battleModel.possibleMoves(selectedSoldier.get, hex) ++ Set(hex) ++
          battleModel.possibleAttacksWhenThereAreNoMoves(selectedSoldier.get, hex)
        battleView.handleEvent(ShowMovementOptions(possible))

      case None => // do nothing
    }
  }

  def rightClickMouse() = withCurrent(currentHex => {
    debug(s"Mouse right clicked in hex (${currentHex.x}, ${currentHex.y})")
    currentHex.soldier match {
      case Some(soldier) => {
        selectedSoldier match {
          case Some(attacker) => {
            val attackerHex = battleModel.hexBySoldier(attacker)

            val hexFromWhichAttack = if (onlyChoiceForSelectedSoldierIsAttack) {
              attackerHex
            } else {
              visitedHexesList.prev.get
            }
            // TODO think if we should remove selected soldier
            selectedSoldier = None

            removeArrow()
            removeDefence()
            removeMovementOptions()

            if (visitedHexesList.isDefined && battleModel.validateMovementAndAttack(attacker, attackerHex,
              hexFromWhichAttack, currentHex, 0)) {

              val attackOpt = selectAttack(attacker, currentHex.soldier.get, hexFromWhichAttack, currentHex)
              attackOpt match {
                case Some(attack) => {
                  if (attackerHex != hexFromWhichAttack) {
                    val moveResult = battleModel.handleMovementEvent(attacker, attackerHex, hexFromWhichAttack)
                    battleView.handleEvent(moveResult.buildBattleViewEvent)
                  }

                  val attackIndex = attacker.soldierType.attacks.indexOf(attack)
                  val attackResult = battleModel.handleEvent(AttackModelEvent(attacker,
                    hexFromWhichAttack, currentHex, attackIndex))
                  battleView.handleEvent(attackResult.buildBattleViewEvent)
                }
                case None => // do nothing
              }
            }
            parent.onMinimapChange()
          }
          case None => // TODO do something, do nothing by now
        }
      }
      case None => {
        selectedSoldier match {
          case Some(soldier) => {
            val from = battleModel.hexBySoldier(soldier)
            if (battleModel.validateMovementEvent(soldier, from, currentHex)) {
              val result = battleModel.handleEvent(MovementModelEvent(soldier, from, currentHex))
              battleView.handleEvent(result.buildBattleViewEvent)
            }

            selectedSoldier = None
            soldierToShow.soldier = None
            removeMovementOptions()
            removeDefence()
            parent.onMinimapChange()
          }
          case None => // do nothing
        }
      }
    }
  })

  def selectAttack(attacker: Soldier, defender: Soldier, attackerHex: TerrainHex,
    defenderHex: TerrainHex): Option[Attack] = parent.showAttackSelectionDialog(attacker, defender, attackerHex, defenderHex)

  def endTurnButton() {
    debug(s"End turn button clicked")
    if (battleModel.validateEndTurn && aiMap.get(battleModel.currentPlayer).isEmpty) {
      val result = battleModel.handleEvent(EndMoveModelEvent)
      battleView.handleEvent(result.buildBattleViewEvent)
      removeArrow()
      removeDefence()
      removeMovementOptions()
      selectedSoldier = None
      soldierToShow.soldier = None
    }
  }

  def leftClickMouse() = withCurrent(hex => {
    debug(s"Mouse left clicked in hex (${hex.x}, ${hex.y})")
    hex.soldier match {
      case Some(soldier) => {
        selectedSoldier = Some(soldier)
        // TODO add if soldier player is human player
        addMovementOptionsForSelectedSoldier()
        removeArrow()
        removeDefence()
      }
      case None => {
        selectedSoldier = None
        removeArrow()
        removeDefence()
        removeMovementOptions()
      }
    }
    soldierToShow.soldier = selectedSoldier

  })

  private def withCurrent[T](f: TerrainHex => T, default: T = Unit): T = {
    val hexOpt = visitedHexesList.current
    hexOpt match {
      case Some(hex) => {
        f(hex)
      }
      case None => default // do nothing
    }
  }

  def endGame = parent.showBattleOverDialog(buildBattleResult)

  private[battle] def canBeCurrentHexAttackedFromPrevious: Boolean = {
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

  private[battle] def onlyChoiceForSelectedSoldierIsAttack: Boolean = withCurrent(hex =>
    selectedSoldier match {
      case Some(soldier) => soldier.movedThisTurn && !soldier.attackedThisTurn &&
        battleModel.validateAttackEvent(soldier, battleModel.hexBySoldier(soldier), hex, 0)
      case None => false
    }, false)

  private def drawArrowForPrevToCurrent() {
    if (visitedHexesList.isDefined) {
      arrowIsShown = true
      battleView.handleEvent(ShowArrow(visitedHexesList.prev.get,
        visitedHexesList.current.get))
    }
  }

  private def drawArrowWhenOnlyChoiceForSelectedSoldierIsAttack() {
    val soldier = selectedSoldier.get
    arrowIsShown = true
    battleView.handleEvent(ShowArrow(battleModel.hexBySoldier(soldier),
      visitedHexesList.current.get))
  }

  private def drawDefenceForHexFromWhichAttack() {
    if (!selectedSoldier.isDefined ||
      visitedHexesList.prev.map(_.soldier.isDefined).getOrElse(true)) {
      return
    }

    val hexFromWhichAttack = visitedHexesList.prev.get
    val soldier = selectedSoldier.get

    defenceIsShown = true
    val defence = battleModel.defenceForSoldier(selectedSoldier.get, hexFromWhichAttack)
    battleView.handleEvent(ShowDefence(hexFromWhichAttack, defence, false))

  }

  private def drawDefenceForSelectedTerrainHex() {
    withCurrent { hex =>
      if (!selectedSoldier.isDefined || hex.soldier.isDefined) {
        return
      }

      defenceIsShown = true
      val defence = battleModel.defenceForSoldier(selectedSoldier.get, hex)
      battleView.handleEvent(ShowDefence(hex, defence, true))
    }
  }

  private var battleIsOverDialogShown = false
  def update(time: Int) {
    battleView.update(time)
    if (!battleModel.isOver) {
      aiMap.get(battleModel.currentPlayer).foreach { ai =>
        if (!battleView.areMovementsGoing) {
          val event = ai.nextTurn(battleModel)
          val result = battleModel.handleEvent(event)
          battleView.handleEvent(result.buildBattleViewEvent)
        }
      }
    } else if (!battleIsOverDialogShown && !battleView.areMovementsGoing
      && battleView.areAllDeadSoldiersNotRendered) {
      battleIsOverDialogShown = true
      parent.showBattleOverDialog(buildBattleResult)
    }

    parent.disableEndTurn.value = !(battleModel.validateEndTurn && aiMap.get(battleModel.currentPlayer).isEmpty)
  }

  def buildBattleResult: BattleResult = {
    val players = battleModel.soldiersByAlliance.head._1
    BattleResult(players.toList)
  }
}

private[battle] class VisitedHexesList {
  private var _prev: Option[TerrainHex] = None
  private var _current: Option[TerrainHex] = None
  def prev = _prev
  def current = _current
  def clean() {
    _prev = None
    _current = None
  }
  def visitHex(hex: TerrainHex) {
    if (_current != Some(hex)) {
      _prev = _current
      _current = Some(hex)
    }
  }
  def isDefined = prev.isDefined && current.isDefined
}