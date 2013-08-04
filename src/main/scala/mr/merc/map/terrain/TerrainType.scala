package mr.merc.map.terrain

object TerrainType {
  val list = List(Water, Grass, Sand, Hill)
  def byName(name:String) = (list ::: helperTypesList).find(_.name.equalsIgnoreCase(name)).get
  val helperTypesList = List(BankInside, BankOutside)
}

abstract sealed class TerrainType(val name:String) {
  def imagePath = "/images/terrain/" + name + ".png"
  
}

case object Grass extends TerrainType("grass")
case object Water extends TerrainType("water")
case object Sand extends TerrainType("sand")
case object Hill extends TerrainType("hill")
case object BankInside extends TerrainType("bankInside")
case object BankOutside extends TerrainType("bankOutside")

