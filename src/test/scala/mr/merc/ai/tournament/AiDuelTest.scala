package mr.merc.ai.tournament

import org.scalatest.FunSuite
import mr.merc.ai.DummyAI
import mr.merc.ai.BattleAI

class AiDuelTest extends FunSuite {
  test("no movement loses to default ai") {
    val predictedWinner = BattleAI()
    val duel = new AiDuel(new DummyAI, predictedWinner)
    duel.playGame()
    assert(duel.winner === Some(duel.player2))
  }
}