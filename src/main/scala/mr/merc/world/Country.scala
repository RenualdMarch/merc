package mr.merc.world

import mr.merc.map.world.Province
import scala.xml.XML

object Country {

}

class Country(val nameKey: String, val culture: Culture) {
  var provinces: Set[Province] = Set()
}