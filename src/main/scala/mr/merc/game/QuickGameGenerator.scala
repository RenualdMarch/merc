package mr.merc.game

import mr.merc.map.generator.RandomTerrainGenerator
import mr.merc.unit.Soldier
import mr.merc.players.Player
import mr.merc.map.GameField
import scalafx.scene.paint.Color

import scala.util.Random
import mr.merc.army.{Warrior, WarriorType}
import mr.merc.economics.{Culture, PoliticalSystem}
import mr.merc.politics.{Party, State}
import mr.merc.util.MercUtils._

class QuickGameGenerator(player1: Player = Player("Human", Color.Yellow),
  player2: Player = Player("AI", Color.Cyan)) extends GameGenerator {

  private val state1 = new State(player1.name, Culture.cultures.randomElement(), 0, new PoliticalSystem(Party.absolute), color = player1.color)
  private val state2 = new State(player2.name, Culture.cultures.randomElement(), 0, new PoliticalSystem(Party.absolute), color = player2.color)

  private def soldiersList(state: State): List[Soldier] = {
    val types = state.primeCulture.warriorViewNames.possibleWarriors.keySet.map(_._1).toList
    val warriors = types.map(wt => new Warrior(wt, WarriorType.Militia, state.primeCulture, state))
    val shuffled = Random.shuffle(warriors)
    shuffled.map(_.soldier)
  }

  override def generateGame: GameField = {
    val field = new RandomTerrainGenerator(0, 0.05).generateMap(20, 20, 0)
    val humans = soldiersList(state1)
    val ais = soldiersList(state2)
    val indices = List(4, 6, 8, 10, 12, 14, 16)
    indices zip humans foreach {
      case (i, s) =>
        field.hex(0, i).soldier = Some(s)
    }

    indices zip ais foreach {
      case (i, s) =>
        field.hex(field.width - 1, i).soldier = Some(s)
    }

    new GameField(field, List(player1, player2), Set(Set(player1), Set(player2)))
  }
}