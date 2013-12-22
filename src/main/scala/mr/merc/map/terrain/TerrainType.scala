package mr.merc.map.terrain

object TerrainType {
  val list = List[TerrainType](Water, Forest, Grass, Sand, Swamp, Hill, Mountain, Road)
  val helperTypesList = List[TerrainType](BankInside, BankOutside)
  private val namesMap = (Village :: list ::: helperTypesList).map(t => (t.name.toLowerCase(), t)).toMap

  def apply(name: String) = namesMap(name.toLowerCase())
}

abstract sealed class TerrainType(val name: String) {
  def imagePath = "/images/terrain/" + name + ".png"

}

case object Grass extends TerrainType("grass")
case object Water extends TerrainType("water")
case object Sand extends TerrainType("sand")
case object Hill extends TerrainType("hill")
case object BankInside extends TerrainType("bankInside")
case object BankOutside extends TerrainType("bankOutside")
case object Swamp extends TerrainType("swamp")
case object Mountain extends TerrainType("mountain")
case object Road extends TerrainType("road")
case object Forest extends TerrainType("forest")

// THIS TYPE IS FORBIDDEN TO USE IN MAP
case object Village extends TerrainType("village")
