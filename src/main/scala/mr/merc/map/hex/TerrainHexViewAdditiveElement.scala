package mr.merc.map.hex

import mr.merc.map.terrain.TerrainType
import scala.collection.mutable.ArrayBuffer
import mr.merc.map.terrain.Grass
import scala.xml.XML
import java.io.File

object TerrainHexViewAdditiveElement {
  private [hex] val elements = new ArrayBuffer[TerrainHexViewAdditiveElement]
  
  parseXml("terrainAdditivesElements.xml")
  
  private [hex] def parseXml(name:String) {
    val path = getClass.getResource("/conf/" + name).toURI
    val xml = XML.loadFile(new File(path))
    val parsed = (xml \ "element").map(node => {
      val terrain = TerrainType.byName((node \ "@type").toString())
      val from = Directions.withName((node \ "@from").toString())
      val to = Directions.withName((node \ "@to").toString())
      new TerrainHexViewAdditiveElement(terrain, from, to)
    })
    
    elements ++= parsed
  }
  
  private [hex] def clean() {
    elements.clear()
  }
}

case class TerrainHexViewAdditiveElement(val terrainType:TerrainType, val from:Directions.Direction, val to:Directions.Direction) {
	private def namePart = if (from == to) {
	  from.toString().toLowerCase()
	} else {
	  from.toString().toLowerCase() + "-" + to.toString().toLowerCase()
	} 
  
  
    def path = "/images/" + terrainType.name + "/" + namePart + ".png"

}