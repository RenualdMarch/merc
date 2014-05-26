package mr.merc.world

import mr.merc.map.world.Province
import scala.xml.XML
import scalafx.scene.paint.Color
import mr.merc.world.character.ComputerCharacter

class Country(val nameKey: String, val culture: Culture, val color: Color) {
  var provinces: Set[Province] = Set()
  var armies: Set[ComputerCharacter] = Set()
  def activeArmies = armies.filter(_.isActive)
}