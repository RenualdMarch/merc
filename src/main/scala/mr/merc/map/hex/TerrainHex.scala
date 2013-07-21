package mr.merc.map.hex

import mr.merc.map.terrain.TerrainType
import mr.merc.map.terrain.Grass

object TerrainHex {
  def grassInit(x:Int, y:Int) = new TerrainHex(x, y, Grass)
}

class TerrainHex(x:Int, y:Int, val terrain:TerrainType) extends Hex(x, y) {

}