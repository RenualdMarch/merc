package mr.merc.world.character

import scalafx.scene.paint.Color
import mr.merc.world.Culture
import mr.merc.unit.SoldierType
import mr.merc.world.Country

class CharacterGenerator {
  def generateHumanCharacter: HumanCharacter = {
    new HumanCharacter("player1", Color.BLUE, Culture("europe"), SoldierType("Human-Horseman"))
  }

  def generateComputerCharacter(nameKey: String, color: Color, charType: CharacterType, culture: Culture, soldierType: SoldierType): Character = {
    new Character(nameKey, color, charType, culture, soldierType)
  }

  def fillCountryWithComputerCharacters(country: Country) {
    ???
  }
}