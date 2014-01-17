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

abstract sealed class TerrainType(val name: String, val layer: Int) {
  def imagePath = "/images/terrain/" + name + ".png"

}

case object Grass extends TerrainType("grass", 0)
case object Water extends TerrainType("water", 0)
case object Sand extends TerrainType("sand", 0)
case object Hill extends TerrainType("hill", 1)
case object BankInside extends TerrainType("bankInside", 0)
case object BankOutside extends TerrainType("bankOutside", 0)
case object Swamp extends TerrainType("swamp", 0)
case object Mountain extends TerrainType("mountain", 2)
case object Road extends TerrainType("road", 0)
case object Forest extends TerrainType("forest", 1)

// THIS TYPES ARE FORBIDDEN TO USE ON MAP
case object Village extends TerrainType("village", 0)
case object Empty extends TerrainType("void", 0)
