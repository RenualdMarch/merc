package mr.merc.map.generator

import mr.merc.map.generator.Biome._
import mr.merc.map.hex._
import mr.merc.map.objects.MapObject
import mr.merc.map.terrain._

import scala.collection.mutable
import scala.math.{abs, max, pow}
import scala.util.Random

object WorldMapGenerator {
  val landPercentage = 0.6

  def generateWorldMap(width: Int, height: Int, provinces: Int):WorldMap = {
    val terrainNoise = Noise(5).add(0.5, Noise(10)).add(0.25, Noise(20)).applyFunction { case ((x, y), n) =>
      val distanceFromCenter = 2*max(abs(x - 0.5), abs(y - 0.5))
      n + 0.6 - 1.6*pow(distanceFromCenter, 2)
    }
    val biomeNoise = Noise(10).add(0.5, Noise(40)).add(0.25, Noise(80))

    def biome(x: Int, y: Int): Biome = {
      biomeNoise(x, width, y, height) match {
        case n if n < biomeNoise.percentageBelow(0.1) => Desert
        case n if n < biomeNoise.percentageBelow(0.6) => Plains
        case n if n < biomeNoise.percentageBelow(0.9) => Trees
        case _ => Rocky
      }
    }

    def f(x: Int, y: Int): TerrainHexInfo = {
      val n = terrainNoise(x, width, y, height)
      if (n > terrainNoise.percentageBelow(1 - landPercentage)) new TerrainHexInfo(x, y, biome(x, y))
      else new TerrainHexInfo(x, y, Sea)
    }

    val field = new HexField[TerrainHexInfo](width, height, f)

    val provincesMap = divideIntoProvinces(field, provinces)
    provincesMap.keys.foreach { h =>
      field.hex(h.x, h.y).biome = CityCastle
    }

    def toTerrainHex(x: Int,y: Int) = new TerrainHex(x, y, field.hex(x, y).terrainType,
      field.hex(x, y).building)

    val terrainField = new TerrainHexField(width, height, toTerrainHex)

   WorldMap(terrainField, provincesMap.map {
      case (capital, hexes) =>
        terrainField.hex(capital.x, capital.y) -> hexes.map(h => terrainField.hex(h.x, h.y))
    })
  }

  def divideIntoProvinces(field: HexField[TerrainHexInfo], provinces: Int):Map[TerrainHexInfo, Set[TerrainHexInfo]] = {
    val totalHexes = field.hexes.filter(_.biome != Sea)
    val firstCapitals = Random.shuffle(totalHexes).take(provinces)
    val division = 0.until(10).foldLeft(MapDivision(firstCapitals.toSet, totalHexes.toSet)) {case (div, _) =>
        val newCapitals = div.lloydRelaxationCapitals
      MapDivision(newCapitals, div.allHexes)
    }

    MapDivision(division.capitals, field.hexes.toSet).voronoiDivision
  }
}

class TerrainHexInfo(x: Int, y: Int, var biome: Biome) extends Hex(x, y) {
  import Biome._
  def terrainType: TerrainType = biome match {
    case Plains => Grass
    case Desert => Sand
    case CityCastle => Castle
    case Rocky => Mountain
    case Trees => Forest
    case Sea => Water
  }

  var building: Option[MapObject] = None
}

sealed trait Biome

object Biome {
  case object Plains extends Biome
  case object Desert extends Biome
  case object Trees extends Biome
  case object CityCastle extends Biome
  //case object Winter extends Biome
  case object Rocky extends Biome
  case object Sea extends Biome
}


case class WorldMap(terrain: TerrainHexField, provinces:Map[TerrainHex, Set[TerrainHex]])

case class MapDivision[T <: Hex](capitals:Set[T], allHexes: Set[T]) {

  def voronoiDivision:Map[T, Set[T]] = {
    val map = capitals.map(c => c -> mutable.Set[T]()).toMap
    allHexes.foreach { h =>
      val closestCapital = capitals.toSeq.minBy(_.distance(h))
      map(closestCapital) += h
    }
    map.transform{case (_, v) => v.toSet}
  }

  def lloydRelaxationCapitals:Set[T] = {
    voronoiDivision.map{case (capital, hexes) =>
      val x = math.round(hexes.toList.map(_.x).sum.toDouble / hexes.size).toInt
      val y = math.round(hexes.toList.map(_.y).sum.toDouble / hexes.size).toInt
      val newCapitalCandidate = new Hex(x, y)
      hexes.minBy(_.distance(newCapitalCandidate))
    } toSet
  }
}
