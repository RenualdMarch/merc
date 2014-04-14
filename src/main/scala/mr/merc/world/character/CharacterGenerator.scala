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

  def generateComputerCharacter(nameKey: String, color: Color, charType: CharacterType, culture: Culture, soldierType: SoldierType) = {
    new ComputerCharacter(nameKey, color, charType, culture, soldierType, 2.5)
  }

  def fillCountryWithComputerCharacters(country: Country) {
    //create governor for each province
    country.provinces.foreach { p =>
      val character = generateComputerCharacter(Random.nextInt.toString, country.color, Governor, country.culture, randomSoldierType(country.culture))
      p.characters.charactersInProvinceCenter += character
    }

    // TODO create generals and mercenaries
    val armies = country.provinces.size / 2
    val list = 0 until armies map { _ =>
      generateComputerCharacter(Random.nextInt.toString, country.color, General, country.culture, randomSoldierType(country.culture))
    } toList

    Random.shuffle(country.provinces).take(armies) zip list foreach {
      case (p, a) =>
        p.characters.charactersInProvinceCenter += a
        country.armies += a
    }
  }

  def randomSoldierType(culture: Culture) = SoldierType("Human-Bowman")
}