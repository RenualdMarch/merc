package mr.merc.game

import mr.merc.map.hex.TerrainHexField
import mr.merc.map.generator.RandomTerrainGenerator
import mr.merc.players.Player
import mr.merc.unit.Soldier
import mr.merc.unit.SoldierType
import mr.merc.players.Player
import mr.merc.map.GameField
import scalafx.scene.paint.Color
import scala.util.Random
import mr.merc.ai.BattleAI

class QuickGameGenerator extends GameGenerator {

  private val types = List("Human-Bowman", "Human-Cavalryman",
    "Human-Fencer", "Human-HeavyInfantryman", "Human-Horseman", "Human-Mage",
    "Human-Spearman")

  private def soldiersList(player: Player): List[Soldier] = {
    val shuffled = Random.shuffle(types)
    shuffled.zipWithIndex.map { case (s, i) => new Soldier(i.toString, SoldierType(s), player) }
  }

  override def generateGame: GameField = {
    val field = new RandomTerrainGenerator(0, 0.05).generateMap(20, 20, 0)
    val humanPlayer = new Player("Human", Color.YELLOW)
    val aiPlayer = new Player("AI", Color.CYAN, Some(BattleAI()))
    val humans = soldiersList(humanPlayer)
    val ais = soldiersList(aiPlayer)
    val indices = List(4, 6, 8, 10, 12, 14, 16)
    indices zip humans foreach {
      case (i, s) =>
        field.hex(0, i).soldier = Some(s)
    }

    indices zip ais foreach {
      case (i, s) =>
        field.hex(19, i).soldier = Some(s)
    }

    new GameField(field, List(humanPlayer, aiPlayer))
  }
}