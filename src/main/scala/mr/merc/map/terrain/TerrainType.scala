package mr.merc.map.terrain

import scalafx.scene.image.Image
import mr.merc.map.hex.Direction
import mr.merc.image.MImage
import javafx.embed.swing.SwingFXUtils

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

// THIS TYPES ARE FORBIDDEN TO USE ON MAP
case object Village extends TerrainType("village")
case object Empty extends TerrainType("void")
