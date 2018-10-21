package mr.merc.map.hex.view

import mr.merc.map.terrain.TerrainType
import scala.xml.XML
import java.io.File
import mr.merc.image.MImage
import scalafx.scene.canvas.GraphicsContext
import mr.merc.map.hex.Direction

object TerrainHexViewAdditiveElement {
  private[hex] var elements = List[TerrainHexViewAdditiveElement]()

  parseXml("terrainAdditivesElements.xml")

  private[hex] def parseXml(name: String) {
    val xml = XML.load(getClass.getResourceAsStream("/conf/" + name))
    val parsed = (xml \ "element").map(node => {
      val terrain = TerrainType((node \ "@type").toString())
      val from = Direction.name((node \ "@from").toString())
      val to = Direction.name((node \ "@to").toString())
      new TerrainHexViewAdditiveElement(terrain, from, to)
    })

    elements = parsed.toList
  }

  def elementsByType(terrain: TerrainType): List[TerrainHexViewAdditiveElement] = {
    val ret = elements.filter(_.terrainType == terrain).sortBy(e => -Direction.length(e.from, e.to))
    require(ret.nonEmpty, s"elements list for terrain type $terrain")
    ret
  }

}

case class TerrainHexViewAdditiveElement(val terrainType: TerrainType, val from: Direction, val to: Direction) {
  private def namePart = if (from == to) {
    from.toString().toLowerCase()
  } else {
    from.toString().toLowerCase() + "-" + to.toString().toLowerCase()
  }

  def path = "/images/terrain/" + terrainType.name + "/" + namePart + ".png"

  private lazy val image = MImage(path)

  def drawItself(gc: GraphicsContext, x: Int, y: Int, factor: Double) {
    image.scaledImage(factor).drawImage(gc, x, y)
  }
}