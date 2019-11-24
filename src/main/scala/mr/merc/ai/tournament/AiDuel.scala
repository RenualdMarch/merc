package mr.merc.ai.tournament

import mr.merc.ai.BattleAI
import mr.merc.game.QuickGameGenerator
import mr.merc.players.Player
import mr.merc.battle.BattleModel

class AiDuel(val first: BattleAI, val second: BattleAI) {
  val player1 = Player("1")
  val player2 = Player("2")
  val game = new QuickGameGenerator(player1, player2).generateGame
  val aiMap = Map(player1 -> first, player2 -> second)
  val model = new BattleModel(game)
  var winner: Option[Player] = None

  def playGame() {
    while (!model.isOver) {
      val player = model.currentPlayer
      val event = aiMap(player).nextTurn(model)
      model.handleEvent(event)
    }

    winner = Some(model.soldiersByAlliance.keys.head.head)
  }
}