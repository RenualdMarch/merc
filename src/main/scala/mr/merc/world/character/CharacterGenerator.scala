package mr.merc.world.character

import scalafx.scene.paint.Color
import mr.merc.world.Culture
import mr.merc.unit.SoldierType
import mr.merc.world.Country
import scala.util.Random

class CharacterGenerator {
  def generateHumanCharacter: HumanCharacter = {
    new HumanCharacter("player1", Color.BLUE, Culture("europe"), SoldierType("Human-Horseman"))
  }

  def generateComputerCharacter(nameKey: String, color: Color, charType: CharacterType, culture: Culture, soldierType: SoldierType): Character = {
    new Character(nameKey, color, charType, culture, soldierType)
  }

  def fillCountryWithComputerCharacters(country: Country) {
    //create governor for each province
    country.provinces.foreach { p =>
      val character = generateComputerCharacter(Random.nextInt.toString, country.color, Governor, country.culture, randomSoldierType(country.culture))
      p.characters.charactersInProvinceCenter += character
    }

    // TODO create generals and mercenaries
  }

  def randomSoldierType(culture: Culture) = SoldierType("Human-Bowman")
}