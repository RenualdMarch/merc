package mr.merc.ai.tournament

import mr.merc.ai.BattleAI
import mr.merc.game.QuickGameGenerator
import mr.merc.players.Player
import mr.merc.battle.BattleModel

class AiDuel(val first: BattleAI, val second: BattleAI) {
  private val player1 = Player("1", ai = Some(first))
  private val player2 = Player("2", ai = Some(second))
  val game = new QuickGameGenerator(player1, player2).generateGame
  val model = new BattleModel(game)
  var winner: Option[Player] = None

  def playGame() {
    while (!model.isOver) {
      val player = model.currentPlayer
      val event = player.ai.get.nextTurn(model)
      model.handleEvent(event)
    }

    winner = Some(model.soldiersByAlliance.keys.toList(0).toList(0))
  }
}