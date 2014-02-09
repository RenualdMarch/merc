package mr.merc.map.reader

import java.io.InputStream
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.terrain.TerrainType
import mr.merc.map.objects.MapObject
import scala.io.Source
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import mr.merc.map.objects._

class WesnothMapReader {
  def readMap(stream: InputStream): TerrainHexField = {
    val lines = Source.fromInputStream(stream).getLines
    val actualLines = lines.drop(3)
    val hexes = actualLines.toArray.map(_.split(","))

    def createHex(x: Int, y: Int): TerrainHex = {
      val cell = parseHex(hexes(y + 1)(x + 1).trim())
      new TerrainHex(x, y, cell._1, cell._2)
    }

    new TerrainHexField(hexes(0).size - 2, hexes.size - 2, createHex)
  }

  def parseHex(cell: String): (TerrainType, Option[MapObject]) = {
    if (cell == "Gs^Fds") {
      return (Forest, None)
    }

    val splittedLines = cell.split("\\^")
    val mapObj = if (splittedLines.size == 2) {
      splittedLines(1) match {
        case "Bw\\" => Some(WoodenBridge)
        case "Vh" => Some(House)
        case x => throw new IllegalArgumentException(s"Unkown map object: $x")
      }
    } else {
      None
    }

    val terrainType = splittedLines(0) match {
      case "Rrc" => Road
      case "Sm" => Swamp
      case "Dd" => Sand
      case "Mm" => Mountain
      case "Wo" => Water
      case "Hh" => Hill
      case "Re" => Dirt
      case "Gg" | "Gs" => Grass
    }

    (terrainType, mapObj)
  }
}