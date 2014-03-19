package mr.merc.world.character

import mr.merc.map.world.Province

class CharactersInProvince {
  var charactersInProvinceCenter: List[Character] = Nil
  var charactersInMovement: Map[Province, List[Character]] = Map()
}