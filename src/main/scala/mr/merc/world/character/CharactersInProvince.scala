package mr.merc.world.character

import mr.merc.map.world.Province
import scala.collection.mutable.ArrayBuffer

class CharactersInProvince {
  val charactersInProvinceCenter: ArrayBuffer[Character] = ArrayBuffer()
  val charactersInMovement: Map[Province, ArrayBuffer[Character]] = Map()
}