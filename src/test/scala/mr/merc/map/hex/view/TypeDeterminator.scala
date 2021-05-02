package mr.merc.map.hex.view

import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain.TerrainType
import mr.merc.map.terrain.TerrainType._

class TypeDeterminator(map: Map[(Int, Int), TerrainType], default: TerrainType = GreenGrass)
  extends Function2[Int, Int, TerrainHex] {

  def apply(x: Int, y: Int): TerrainHex = {
    val ter = map.get((x, y))
    val terType = ter match {
      case Some(t) => t
      case None => default
    }

    new TerrainHex(x, y, terType)
  }
}
