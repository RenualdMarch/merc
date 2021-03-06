package mr.merc.battle

import mr.merc.map.GameField
import mr.merc.battle.event._
import mr.merc.unit.Soldier
import mr.merc.map.hex.TerrainHex
import mr.merc.map.pathfind.PathFinder
import mr.merc.unit.Attack
import mr.merc.map.pathfind.PossibleMovesFinder
import mr.merc.unit.BeforeTurnAction
import mr.merc.unit.SoldierTurnState
import mr.merc.unit.NotHisTurn
import mr.merc.unit.HaveAttacked
import mr.merc.unit.CanntMoveAnyMore
import mr.merc.unit.StillCanMove
import mr.merc.unit.HaventMoved
import mr.merc.log.Logging
import mr.merc.players.Player

class BattleModel(val map: GameField) extends BattleModelEventHandler with Logging {
  private val sides = map.sides
  private var currentPlayerIndex = 0
  def currentPlayer = map.players(currentPlayerIndex)
  private var prevSoldiersWithHexes: Option[List[(Soldier, TerrainHex)]] = None
  map.hexField.soldierChangeListener = (_, _) => { prevSoldiersWithHexes = None }
  def allSoldiersWithHexes = {
    prevSoldiersWithHexes match {
      case Some(soldiers) => soldiers
      case None =>
        val sh = map.hexField.hexes.filter(_.soldier.isDefined).map(h => (h.soldier.get, h)).toList
        prevSoldiersWithHexes = Some(sh)
        prevSoldiersWithHexes.get

    }
  }
  def allSoldiers = allSoldiersWithHexes.map(_._1)
  setSoldierTurnState()

  def handleEvent(event: BattleModelEvent): BattleModelEventResult = {
    info(s"Battle model recevied event $event")
    event match {
      case MovementModelEvent(soldier, from, to) => handleMovementEvent(soldier, from, to)
      case AttackModelEvent(soldier, from, target, attackNumber) => handleAttackEvent(soldier, from, target, attackNumber)
      case EndMoveModelEvent => handleEndTurnEvent()
    }
  }

  private[battle] def handleMovementEvent(soldier: Soldier, from: TerrainHex, to: TerrainHex): MovementModelEventResult = {
    require(validateMovementEvent(soldier, from, to), s"movement from $from to $to is invalid")
    require(to.soldier.isEmpty)
    val path = PathFinder.findPath(map.gridForSoldier(soldier), from, to)
    val movePrice = pathPrice(soldier.movementCostFunction, path.get)
    soldier.movePointsRemain -= movePrice
    from.soldier = None
    to.soldier = Some(soldier)
    soldier.turnState = soldierTurnState(to)

    MovementModelEventResult(soldier, path.get)
  }

  private def setSoldierTurnState() {
    map.hexField.hexes.filter(_.soldier.isDefined).map(h => (h, h.soldier.get)).foreach {
      case (hex, soldier) =>
        soldier.turnState = soldierTurnState(hex)
    }
  }

  def areFriends(player1:Player, player2:Player):Boolean = {
    player1 == player2 ||
      sides.exists(set => set.contains(player1) && set.contains(player2))
  }

  def areEnemies(player1:Player, player2:Player):Boolean = {
    !areFriends(player1, player2)
  }

  private[battle] def handleEndTurnEvent(): EndMoveModelEventResult = {
    require(validateEndTurn)
    nextPlayer()
    val beforeTurnActions = map.hexField.hexes.filter(_.soldier.isDefined).map(h => (h.x, h.y, h.soldier.get)).
      filter(_._3.owner == currentPlayer).flatMap { case (x, y, s) => s.beforeTurnActions(map.hexField, x, y) }
    val filteredActions = BeforeTurnAction.filterActions(beforeTurnActions.toSet)
    filteredActions foreach (_.action())
    allSoldiers.filter(_.owner == currentPlayer).foreach(_.beforeTurnRenowation())
    setSoldierTurnState()
    EndMoveModelEventResult(currentPlayer)
  }

  private[battle] def handleAttackEvent(soldier: Soldier, from: TerrainHex, target: TerrainHex, attackNumber: Int): AttackModelEventResult = {
    require(validateAttackEvent(soldier, from, target, attackNumber), s"invalid attack attempt from $from to $target with attack $attackNumber")
    val defender = target.soldier.get

    val attackerAttack = soldier.soldierType.attacks(attackNumber)
    val defenderAttack = Attack.selectBestAttackForDefender(soldier, defender, attackerAttack)
    soldier.attackedThisTurn = true
    val result = Attack.battle(from, target, attackerAttack, defenderAttack)
    if (soldier.hp == 0) {
      from.soldier = None
    } else if (defender.hp == 0) {
      target.soldier = None
    }

    if (soldier.hp != 0) {
      soldier.turnState = soldierTurnState(from)
    }
    new AttackModelEventResult(from, target, soldier, defender, result)
  }

  def hexBySoldier(soldierOpt: Option[Soldier]): Option[TerrainHex] = soldierOpt map hexBySoldier

  def hexBySoldier(soldier: Soldier) = map.hexField.hexes.find(h => h.soldier == Some(soldier)).get

  def validateMovementEvent(soldier: Soldier, from: TerrainHex, to: TerrainHex, validatePath: Boolean = true, checkPlayer: Boolean = true): Boolean = {
    if (checkPlayer && !(soldier.owner == currentPlayer)) {
      return false
    }

    if (from == to) {
      return false
    }

    if (soldier.attackedThisTurn) {
      return false
    }

    if (soldier.movedThisTurn && map.zoneOfControlForEnemys(soldier.owner).contains(from)) {
      return false
    }

    if (validatePath) {
      val pathOpt = PathFinder.findOptimalPath(map.gridForSoldier(soldier), from, to, soldier.movePointsRemain, soldier.movedThisTurn)
      pathOpt match {
        case Some(path) => {
          val price = pathPrice(soldier.movementCostFunction, path)
          price <= soldier.movePointsRemain
        }
        case None => false
      }
    } else {
      true
    }
  }

  def possibleMoves(soldier: Soldier, currentHex: TerrainHex): Set[TerrainHex] = {
    val movePoints = if (soldier.owner == currentPlayer) {
      soldier.movePointsRemain
    } else {
      soldier.soldierType.movement
    }

    val movedThisTurn = if (soldier.owner == currentPlayer) {
      soldier.movedThisTurn
    } else {
      false
    }

    PossibleMovesFinder.findPossibleMoves(map.gridForSoldier(soldier), currentHex,
      movePoints, movedThisTurn).filterNot(h => h == currentHex || h.soldier.nonEmpty)
  }

  def possibleAttacksWhenThereAreNoMoves(soldier: Soldier, currentHex: TerrainHex): Set[TerrainHex] = {
    val neigbours = map.hexField.neighboursSet(currentHex)
    val enemiesNear = neigbours.filter(_.soldier.exists(s => areEnemies(s.owner, soldier.owner)))
    if (enemiesNear.isEmpty) {
      return Set()
    }

    if (soldier.movedThisTurn) {
      enemiesNear
    } else {
      Set()
    }
  }

  // TODO add test
  def validateMovementAndAttack(soldier: Soldier, start: TerrainHex, destination: TerrainHex, underAttack: TerrainHex, attackNumber: Int): Boolean = {
    if (start != destination) {
      validateMovementEvent(soldier, start, destination) && validateAttackEvent(soldier, destination, underAttack, attackNumber)
    } else {
      validateAttackEvent(soldier, destination, underAttack, attackNumber)
    }
  }

  // TODO allies test
  def validateAttackEvent(soldier: Soldier, from: TerrainHex, target: TerrainHex, attackNumber: Int): Boolean = {
    if (soldier.attackedThisTurn) {
      return false
    }

    if (soldier.owner != currentPlayer) {
      return false
    }

    if (soldier.soldierType.attacks.size <= attackNumber) {
      return false
    }

    if (!map.hexField.neighbours(from).contains(target)) {
      return false
    }

    target.soldier match {
      case Some(enemy) => areEnemies(soldier.owner, enemy.owner)
      case None => false
    }
  }

  def validateEndTurn = !isOver

  private def nextPlayer() {
    currentPlayerIndex += 1
    if (currentPlayerIndex == map.players.size) {
      currentPlayerIndex = 0
    }
  }

  private def pathPrice(cost: TerrainHex => Int, path: List[TerrainHex]): Int = {
    path.tail.map(cost).sum
  }

  private[battle] def soldierTurnState(hex: TerrainHex): SoldierTurnState = {
    val soldier = hex.soldier.get
    if (soldier.owner != currentPlayer) {
      return NotHisTurn
    }

    if (soldier.attackedThisTurn) {
      return HaveAttacked
    }

    val moves = possibleMoves(soldier, hex)
    if (moves.isEmpty) {
      CanntMoveAnyMore
    } else if (soldier.movedThisTurn) {
      StillCanMove
    } else {
      HaventMoved
    }
  }

  def isOver: Boolean = soldiersByAlliance.size == 1

  def soldiersByAlliance: Map[Set[Player], List[Soldier]] = {
    allSoldiers.groupBy(s => sides.find(_.contains(s.owner)).get)
  }

  def defenceForSoldier(soldier: Soldier, hex: TerrainHex) = Attack.calculateSoldierDefence(soldier, hex)
}

case class BattleResult(winningAlliance: List[Player])