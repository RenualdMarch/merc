package mr.merc.main

import mr.merc.ai.BattleAI
import mr.merc.army.{Warrior, WarriorCompetence, WarriorType}
import mr.merc.battle.BattleModel
import mr.merc.economics.{Culture, PoliticalSystem}
import mr.merc.map.GameField
import mr.merc.map.generator.RandomTerrainGenerator
import mr.merc.politics.{Party, State}
import mr.merc.economics.MapUtil.NumericOperations._

import scala.util.Random

object UnitsBalance {

  private case class BattleResult(winner: WarriorType, loser: WarriorType, wc: WarriorCompetence)

  def main(args: Array[String]): Unit = {
    val rounds = 50
    val r = tournament(WarriorType.allWarriorTypes.toVector, WarriorCompetence.Militia, rounds)
    printResults(r, rounds)
  }

  private def printResults(map:Map[WarriorType, Int], rounds:Int): Unit = {
    val maxPoints = rounds * (map.size - 1)
    map.toList.sortBy(_._2).zipWithIndex.foreach { case ((wt, result), index) =>
      println(s"$index $wt ${result.toDouble / maxPoints * 100}")
    }
  }

  private def tournament(warriorTypes: Vector[WarriorType], competence: WarriorCompetence, rounds: Int): Map[WarriorType, Int] = {
    (0 until rounds).par.map(_ => tournamentRound(warriorTypes, competence)).reduce(_ |+| _)
  }

  private def tournamentRound(warriorTypes: Vector[WarriorType], competence: WarriorCompetence): Map[WarriorType, Int] = {
    val results = for {
      i <- warriorTypes.indices
      j <- warriorTypes.indices if i < j
    } yield {
      tournamentBattle(warriorTypes(i), warriorTypes(j), competence)
    }
    results.groupBy(_.winner).map { case (wc, list) => wc -> list.size
    }
  }

  private def tournamentBattle(wt1: WarriorType, wt2: WarriorType, wc: WarriorCompetence): BattleResult = {
    val field = new RandomTerrainGenerator(0, 0.05).generateMap(12, 12, Random.nextInt())

    val culture1 = cultureByWarriorType(wt1, wc)
    val state1 = new State("1", culture1, 0, new PoliticalSystem(Party.absolute))
    val player1 = state1.toPlayer

    val culture2 = cultureByWarriorType(wt2, wc)
    val state2 = new State("2", culture2, 0, new PoliticalSystem(Party.absolute))
    val player2 = state2.toPlayer

    val warrior1 = new Warrior(wt1, wc, culture1, state1)
    val warrior2 = new Warrior(wt2, wc, culture2, state2)
    field.hex(0, 0).soldier = Some(warrior1.soldier)
    field.hex(10, 10).soldier = Some(warrior2.soldier)

    val gameField = new GameField(field, List(player1, player2),
      Set(Set(player1), Set(player2)))
    val aiMap = Map(player1 -> BattleAI(), player2 -> BattleAI())
    val model = new BattleModel(gameField)

    val maxCount = 10000
    var count = 0

    while (!model.isOver && count < maxCount) {
      val player = model.currentPlayer
      val event = aiMap(player).nextTurn(model)
      count += 1
      model.handleEvent(event)
    }

    if (count == maxCount) {
      tournamentBattle(wt1, wt2, wc)
    } else {
      val winner = model.soldiersByAlliance.keys.head.head
      if (winner == player1)
        BattleResult(wt1, wt2, wc)
      else if (winner == player2)
        BattleResult(wt2, wt1, wc)
      else sys.error(s"Expected player1 or player2 but got $winner")
    }
  }

  private def cultureByWarriorType(wt: WarriorType, wc: WarriorCompetence): Culture = {
    Culture.cultures.find { c =>
      c.warriorViewNames.possibleWarriors.contains(wt, wc)
    }
    }.get

}