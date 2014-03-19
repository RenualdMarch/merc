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
import mr.merc.view.move.MomentaryMovement
import mr.merc.view.move.MomentaryMovement
import mr.merc.unit.view._
import mr.merc.log.Logging
import scalafx.geometry.Rectangle2D
import mr.merc.view.move.SmoothMovement

// injecting soldier drawer for test purposes only
class BattleView(model: BattleModel, soldierDrawer: SoldiersDrawer[SoldierView] = new SoldiersDrawer) extends Logging {
  val mapView = new MapView(model.map.hexField, soldierDrawer)

  private val hexesViewMap = mapView.terrainView.realHexes.map(v => (v.hex -> v)) toMap
  private val soldiersMap = mapView.soldiers.map(s => (s.soldier -> s)) toMap

  def update(time: Int) {
    mapView.update(time)
  }

  def hexByPixel(x: Int, y: Int, viewport: Rectangle2D) = mapView.hexByPixel(x + viewport.minX.toInt, y + viewport.minY.toInt)

  def wrap(s: Soldier) = soldiersMap(s)

  def wrap(t: TerrainHex) = hexesViewMap(t)

  def areMovementsGoing = soldierDrawer.movements.nonEmpty

  def areAllDeadSoldiersNotRendered = soldierDrawer.soldiers.forall { s =>
    s.soldier.hp != 0 || (s.soldier.hp == 0 && s.state == NoState)
  }

  def handleEvent(event: BattleViewEvent) {
    info(s"Battle view received event $event")
    event match {
      case AttackBattleViewEvent(attackerTerrainHex, defenderTerrainHex, attacker, defender, result) =>
        handleAttackEvent(wrap(attackerTerrainHex), wrap(defenderTerrainHex), attacker, defender, result)
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
      case ShowDefence(view, defence, drawPolygon) => {
        mapView.terrainView.defence = Some(wrap(view), defence, drawPolygon)
      }
      case HideDefence => {
        mapView.terrainView.defence = None
      }
    }
  }

  private def handleAttackEvent(attackerTerrainHex: TerrainHexView,
    defenderTerrainHex: TerrainHexView, attacker: Soldier, defender: Soldier, result: List[AttackResult]) {
    val dir = model.map.hexField.direction(attackerTerrainHex.hex, defenderTerrainHex.hex)

    def factory(res: AttackResult) = {
      val currentAttacker = res.attacker
      val currendDefender = res.defender
      val attack = res.attackersAttack
      val ranged = attack.ranged
      val attackNumber = res.attacker.soldierType.attacks.indexOf(attack)

      val attackPos = if (attacker == currentAttacker) attackerTerrainHex else defenderTerrainHex
      val defenderPos = if (defender == currendDefender) defenderTerrainHex else attackerTerrainHex
      val currentDirection = if (attacker == currentAttacker) dir else dir.opposite

      if (ranged) {
        new SoldierRangedAttackMovement(attackPos, defenderPos, currentDirection,
          wrap(currentAttacker), wrap(currendDefender), res, mapView.terrainView)
      } else {
        new SoldierAttackMovement(attackPos, defenderPos, currentDirection,
          wrap(currentAttacker), wrap(currendDefender), res, mapView.terrainView)
      }
    }

    val attacksList = result map factory
    val refreshList = List(new MomentaryMovement(battleOverActions(attacker)),
      new MomentaryMovement(battleOverActions(defender)))
    val move = new MovementList(attacksList ++ refreshList)
    mapView.addMovement(move)
  }

  private def battleOverActions(soldier: Soldier) {
    val view = wrap(soldier)
    view.refreshBars()
    if (soldier.hp == 0) {
      view.state = DeathState
    }
  }

  private def handleMovementEvent(soldier: SoldierView, path: List[TerrainHexView]) {
    val standAfterMove = MomentaryMovement(soldier.state = StandState)
    mapView.addMovement(new SmoothMovement(path, soldier, mapView.terrainView))
    mapView.addMovement(standAfterMove)
  }

  private def handleEndMoveEvent(player: Player) {
    // do nothing by now
  }
}