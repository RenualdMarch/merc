package mr.merc.world

import mr.merc.map.world.Province
import scala.xml.XML
import scalafx.scene.paint.Color

class Country(val nameKey: String, val culture: Culture, val color: Color) {
  var provinces: Set[Province] = Set()
}