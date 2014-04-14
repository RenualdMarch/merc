package mr.merc.world.character

import mr.merc.unit.SoldierType
import mr.merc.world.Culture
import scalafx.scene.paint.Color

class ComputerCharacter(nameKey: String, color: Color, characterType: CharacterType, culture: Culture, soldierType: SoldierType, val power: Double)
  extends Character(nameKey, color, characterType, culture, soldierType) {
}