package mr.merc.world

import mr.merc.map.world.Province

class Country(val nameKey: String, val culture: Culture) {
  var provinces: Set[Province] = Set()
}