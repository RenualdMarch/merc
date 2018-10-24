package mr.merc.map.hex.view

import mr.merc.map.terrain.TerrainType

import mr.merc.image.MImage
import scalafx.scene.canvas.GraphicsContext
import mr.merc.map.hex._

object TerrainHexViewAdditiveElement {
  private val vector = List(N, NE, SE, S, SW, NW)
  private val possibleCombinations = for {
    from <- vector
    to <- vector
  } yield {
    (from, to)
  }

  private[hex] val elements = scanElements()

  def fileName(tt: TerrainType, x:(Direction, Direction)):String = {
    val (from, to) = x
    if (from == to) {
      "/images/terrain/" + tt.name + "/" + from.name.toLowerCase + ".png"
    } else {
      "/images/terrain/" + tt.name + "/" + from.name.toLowerCase + "-" + to.name.toLowerCase + ".png"
    }
  }

  def scanElements():Map[TerrainType, List[TerrainHexViewAdditiveElement]] = {
    (TerrainType.list ::: TerrainType.helperTypesList) map { t =>
      t -> possibleCombinations.flatMap { case x@(from, to) =>
        val name = fileName(t, x)
        Option(getClass.getResource(name)).map {_ =>
          TerrainHexViewAdditiveElement(t, from, to)
        }
      }
    } toMap
  }
}

case class TerrainHexViewAdditiveElement(terrainType: TerrainType, from: Direction, to: Direction) {
  private def namePart = if (from == to) {
    from.toString().toLowerCase()
  } else {
    from.toString().toLowerCase() + "-" + to.toString().toLowerCase()
  }

  def path:String = "/images/terrain/" + terrainType.name + "/" + namePart + ".png"

  private lazy val image = MImage(path)

  def drawItself(gc: GraphicsContext, x: Int, y: Int, factor: Double) {
    image.scaledImage(factor).drawImage(gc, x, y)
  }
}