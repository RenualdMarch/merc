package mr.merc.world.character

import mr.merc.unit.SoldierType
import mr.merc.world.Culture
import scalafx.scene.paint.Color

class ComputerCharacter(nameKey: String, color: Color, characterType: CharacterType, culture: Culture, soldierType: SoldierType, val power: Double)
  extends Character(nameKey, color, characterType, culture, soldierType) {

  private var turnsBeforeActivation: Option[Int] = None
  def isActive = turnsBeforeActivation.isEmpty
  def deactivate(turns: Int) {
    turnsBeforeActivation = Some(turns)
  }
  def turnEnd() {
    turnsBeforeActivation = turnsBeforeActivation match {
      case Some(i) => if (i == 1) None else Some(i - 1)
      case None => None
    }
  }
}