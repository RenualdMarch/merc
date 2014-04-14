package mr.merc.world.character

import mr.merc.local.Localization
import scalafx.scene.paint.Color
import mr.merc.world.Culture
import mr.merc.unit.SoldierType

abstract class Character(nameKey: String, _color: Color, val characterType: CharacterType, val culture: Culture, val soldierType: SoldierType) {

  def color = _color
  val name = Localization(nameKey)
}

sealed trait CharacterType {
  val nameKey: String
  val name = Localization(nameKey)
}

case object General extends CharacterType {
  val nameKey = "character.general"
}

case object Governor extends CharacterType {
  val nameKey = "character.governor"
}

case object MercenaryGeneral extends CharacterType {
  val nameKey = "character.mercenary"
}
