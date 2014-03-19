package mr.merc.world.character

import scalafx.scene.paint.Color
import mr.merc.unit.SoldierType
import mr.merc.world.Culture

class HumanCharacter(nameKey: String, color: Color, culture: Culture, soldierType: SoldierType) extends Character(nameKey, color, MercenaryGeneral, culture, soldierType) {
  override def name = nameKey
}