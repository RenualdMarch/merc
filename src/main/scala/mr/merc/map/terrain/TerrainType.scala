package mr.merc.map.terrain

object TerrainType {
  val list = List(Grass, Sand, Hill)
  def byName(name:String) = list.find(_.name.equalsIgnoreCase(name)).get
}

abstract sealed class TerrainType(val name:String) {
  def imagePath = "/images/terrain/" + name + ".png"
  
}

case object Grass extends TerrainType("grass")
case object Sand extends TerrainType("sand")
case object Hill extends TerrainType("hill")
