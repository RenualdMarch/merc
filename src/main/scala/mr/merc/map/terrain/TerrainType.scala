package mr.merc.map.terrain

abstract sealed class TerrainType(val name:String) {
  def imagePath = "/images/terrain/" + name + ".png"
}

case object Grass extends TerrainType("grass")
case object Sand extends TerrainType("sand")
case object Hill extends TerrainType("hill")
