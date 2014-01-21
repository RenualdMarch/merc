package mr.merc.game

import mr.merc.map.GameField

trait GameGenerator {
  def generateGame: GameField
}