package mr.merc.world.character

import mr.merc.local.Localization
import scalafx.scene.paint.Color
import mr.merc.world.Culture
import mr.merc.unit.SoldierType

class Character(nameKey: String, _color: Color, val characterType: CharacterType, culture: Culture, soldierType: SoldierType) {

  def color = _color
  def name = Localization(nameKey)
}

sealed trait CharacterType
case object General extends CharacterType
case object Governor extends CharacterType
case object MercenaryGeneral extends CharacterType
case object SoldierCharacter extends CharacterType