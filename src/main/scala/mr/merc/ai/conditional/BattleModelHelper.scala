package mr.merc.ai.conditional

import mr.merc.battle.BattleModel
import mr.merc.unit.Soldier
import mr.merc.map.hex.TerrainHex

object BattleModelHelper {
  implicit class model2helper(model: BattleModel) {
    def currentSoldiers = model.allSoldiersWithHexes.filter(_._1.owner.isSamePlayer(model.currentPlayer))

    def enemies = model.allSoldiersWithHexes.filter(_._1.owner.isEnemy(model.currentPlayer))

    def friends = model.allSoldiersWithHexes.filter(_._1.owner.isFriend(model.currentPlayer))

    def movedCurrentSoldiers = currentSoldiers.map(_._1).filter(_.movedThisTurn)

    def reachableEnemies(hex: TerrainHex, soldier: Soldier): Set[TerrainHex] = {
      val reachableNeigs = model.possibleMoves(soldier, hex).flatMap(h => model.map.hexField.neighbours(h))
      reachableNeigs.filter(_.soldier.isDefined).filter(_.soldier.get.owner.isEnemy(soldier.owner))
    }
  }
}