package mr.merc.map.hex

import mr.merc.map.terrain.TerrainType
import scala.collection.mutable.ArrayBuffer
import mr.merc.map.terrain.Grass
import scala.xml.XML
import java.io.File
import mr.merc.image.MImage
import scalafx.scene.canvas.GraphicsContext

object TerrainHexViewAdditiveElement {
  private [hex] var elements = List[TerrainHexViewAdditiveElement]()
  
  parseXml("terrainAdditivesElements.xml")
  
  private [hex] def parseXml(name:String) {
    val path = getClass.getResource("/conf/" + name).toURI
    val xml = XML.loadFile(new File(path))
    val parsed = (xml \ "element").map(node => {
      val terrain = TerrainType((node \ "@type").toString())
      val from = Directions.withName((node \ "@from").toString())
      val to = Directions.withName((node \ "@to").toString())
      new TerrainHexViewAdditiveElement(terrain, from, to)
    })
    
    elements = parsed.toList
  }
  
  def elementsByType(terrain:TerrainType):List[TerrainHexViewAdditiveElement] = {
    val ret = elements.filter(_.terrainType == terrain).sortBy(e => -Directions.length(e.from, e.to))
    require(!ret.isEmpty, s"elements list for terrain type $terrain")
    ret
  }

}

case class TerrainHexViewAdditiveElement(val terrainType:TerrainType, val from:Directions.Direction, val to:Directions.Direction) {
	private def namePart = if (from == to) {
	  from.toString().toLowerCase()
	} else {
	  from.toString().toLowerCase() + "-" + to.toString().toLowerCase()
	} 
  
  
    def path = "/images/terrain/" + terrainType.name + "/" + namePart + ".png"

    private lazy val image = MImage(path)
    
    def drawItself(gc:GraphicsContext, x:Int, y:Int) {
      image.drawImage(gc, x, y)
    }
}