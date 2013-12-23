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

// injecting soldier drawer for test purposes only
class BattleView(model: BattleModel, _soldierDrawer: SoldiersDrawer = new SoldiersDrawer) {
  val mapView = new MapView(model.map.hexField, _soldierDrawer)

  private val hexesViewMap = mapView.terrainView.hexes.map(v => (v.hex -> v)) toMap
  private val soldiersMap = mapView.soldiers.map(s => (s.soldier -> s)) toMap

  def update(time: Int) {
    mapView.update(time)
  }

  def drawItself(gc: GraphicsContext) {
    mapView.drawItself(gc)
  }

  def hexByPixel(x: Int, y: Int) = mapView.hexByPixel(x, y)

  def wrap(s: Soldier) = soldiersMap(s)

  def wrap(t: TerrainHex) = hexesViewMap(t)

  def handleEvent(event: BattleViewEvent) {
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

      val attackPos = if (attacker == currentAttacker) attackerTerrainHex.coords else defenderTerrainHex.coords
      val defenderPos = if (defender == currendDefender) defenderTerrainHex.coords else attackerTerrainHex.coords
      val currentDirection = if (attacker == currentAttacker) dir else dir.opposite

      if (ranged) {
        new SoldierRangedAttackMovement(attackPos, defenderPos, currentDirection,
          wrap(currentAttacker), wrap(currendDefender), res)
      } else {
        new SoldierAttackMovement(attackPos, defenderPos, currentDirection,
          wrap(currentAttacker), wrap(currendDefender), res)
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
    val destination = path.tail
    val departure = path.init
    val allMovements = for ((a, b) <- departure zip destination) yield new SoldierMoveMovement(a, b, soldier)
    val standAfterMove = MomentaryMovement(soldier.state = StandState)
    mapView.addMovement(new MovementList(allMovements :+ standAfterMove))
  }

  private def handleEndMoveEvent(player: Player) {
    // do nothing by now
  }
}