package mr.merc.map.generator

import mr.merc.economics.{FourSeasonsTerrainHex, FourSeasonsTerrainHexField, WorldGenerator}
import mr.merc.map.ShortestGrid
import mr.merc.map.hex._
import mr.merc.map.pathfind.PathFinder
import mr.merc.map.terrain._

import scala.collection.mutable
import scala.math.{abs, max, pow}
import scala.util.Random
import mr.merc.economics.WorldGenerationConstants._
import FourSeasonsTerrainTypes._
import FourSeasonsMapObjects._
import mr.merc.util.MercUtils

object WorldMapGenerator {


  def generateWorldMap(width: Int, height: Int, provinces: Int): WorldMap = {
    val terrainNoise = Noise(5).add(0.5, Noise(10)).add(0.25, Noise(20)).applyFunction { case ((x, y), n) =>
      val distanceFromCenter = 2 * max(abs(x - 0.5), abs(y - 0.5))
      n + 0.6 - 1.6 * pow(distanceFromCenter, 2)
    }

    val biomeNoise = Noise(10).add(0.5, Noise(40)).add(0.25, Noise(80))

    def biome(x: Int, y: Int): FourSeasonsTerrainType = {
      biomeNoise(x, width, y, height) match {
        //case n if n < biomeNoise.percentageBelow(0.2) => DesertSand
        case n if n < biomeNoise.percentageBelow(0.6) => FourSeasonsGrass
        case n if n < biomeNoise.percentageBelow(0.85) => FourSeasonsDecForest
        case n if n < biomeNoise.percentageBelow(0.98) => FourSeasonsHill
        case _ => FourSeasonsMountain
      }
    }

    def f(x: Int, y: Int): FourSeasonsTerrainHex = {
      val n = terrainNoise(x, width, y, height)
      if (n > terrainNoise.percentageBelow(1 - LandPercentage)) new FourSeasonsTerrainHex(x, y, biome(x, y))
      else new FourSeasonsTerrainHex(x, y, FourSeasonsWater)
    }

    val terrainField = new FourSeasonsTerrainHexField(width, height, f)

    val provincesMap = divideIntoProvinces(terrainField, provinces)
    provincesMap.keys.foreach { h =>
      terrainField.hex(h.x, h.y).terrainMap = FourSeasonsCastle
    }

    addRivers(terrainField)
    connectCitiesByRoads(terrainField, provincesMap)
    provincesMap.keys.foreach { cap =>
      makeRoadAroundCapitals(terrainField, cap)
    }
    addFlowers(terrainField)

    WorldMap(terrainField, provincesMap.map {
      case (capital, hexes) =>
        terrainField.hex(capital.x, capital.y) -> hexes.map(h => terrainField.hex(h.x, h.y))
    })
  }

  def divideIntoProvinces(field: HexField[FourSeasonsTerrainHex], provinces: Int): Map[FourSeasonsTerrainHex, Set[FourSeasonsTerrainHex]] = {
    val totalHexes = field.hexes.filterNot(_.terrainMap == FourSeasonsWater)
    val firstCapitals = Random.shuffle(totalHexes).take(provinces)
    val division = 0.until(10).foldLeft(MapDivision(firstCapitals.toSet, totalHexes.toSet)) { case (div, _) =>
      val newCapitals = div.lloydRelaxationCapitals
      MapDivision(newCapitals, div.allHexes)
    }

    MapDivision(division.capitals, field.hexes.toSet).voronoiDivision
  }

  def addRivers(field: FourSeasonsTerrainHexField): Unit = {
    val riversCount = (field.width + field.height) / 3
    val hexesPerCurve = 4

    val initialRivers = (0 until riversCount).par.flatMap { _ =>
      val x = Random.nextInt(field.width)
      val y = Random.nextInt(field.height - 1)
      val from = field.hex(x, y)

      field.findClosest(from, x => x.terrainMap == FourSeasonsWater || x.terrainMap == FourSeasonsRiver).flatMap { target =>
        field.findPath(from, target, h => h.terrainMap == FourSeasonsCastle ||
          field.neighbours(h).exists(_.terrainMap == FourSeasonsCastle))
      }
    }
    val blocks = initialRivers.flatMap { river =>
      river.drop(hexesPerCurve - 1).grouped(hexesPerCurve).map(_.head)
    }.toSet

    val initialRiversHexes = initialRivers.flatten.toSet.seq -- blocks

    val pathGrid = new ShortestGrid[FourSeasonsTerrainHex] {
      override def heuristic(from: FourSeasonsTerrainHex, to: FourSeasonsTerrainHex): Double = {
        math.abs(from.x - to.x) + math.abs(from.y - to.y)
      }

      override def isBlocked(t: FourSeasonsTerrainHex): Boolean = t.terrainMap == FourSeasonsCastle || neighbours(t).exists(_.terrainMap == FourSeasonsCastle) || blocks.contains(t)

      override def price(from: FourSeasonsTerrainHex, to: FourSeasonsTerrainHex): Double = if (initialRiversHexes.contains(to)) 0.1 else 1

      override def neighbours(t: FourSeasonsTerrainHex): List[FourSeasonsTerrainHex] = field.neighbours(t)
    }

    val riverHexes = initialRivers.par.flatMap { river =>
      val head = river.head
      val last = river.last
      PathFinder.findPath(pathGrid, head, last).toList.flatten
    }

    riverHexes.foreach { h =>
      h.terrainMap = FourSeasonsRiver
    }
  }

  def connectCitiesByRoads(field: FourSeasonsTerrainHexField, provincesMap: Map[FourSeasonsTerrainHex, Set[FourSeasonsTerrainHex]]): Unit = {
    val connectivityMap = WorldGenerator.buildConnectivityMap(field, provincesMap)
    val connections = connectivityMap.flatMap { case (capital, neigs) =>
      neigs.map(h => Set(capital, h))
    }.toSet

    connections.map(_.toList).foreach { case List(from, to) =>
      val grid = new ShortestGrid[FourSeasonsTerrainHex] {
        override def heuristic(from: FourSeasonsTerrainHex, to: FourSeasonsTerrainHex): Double = price(from, to)

        override def isBlocked(t: FourSeasonsTerrainHex): Boolean = !provincesMap(from).contains(t) && !provincesMap(to).contains(t)

        override def price(from: FourSeasonsTerrainHex, to: FourSeasonsTerrainHex): Double =
          if (to.terrainMap == FourSeasonsRoad) 0.5
          else if (to.mapObj.contains(FourSeasonsWoodenBridge)) 0.7
          else if (to.terrainMap == FourSeasonsWater || to.terrainMap == FourSeasonsRiver) 3
          else 1

        override def neighbours(t: FourSeasonsTerrainHex): List[FourSeasonsTerrainHex] = field.neighbours(t)
      }

      PathFinder.findPath(grid, from, to).foreach { path =>
        path.foreach { h =>
          if (h.terrainMap == FourSeasonsWater || h.terrainMap == FourSeasonsRiver) {
            h.mapObj = Some(FourSeasonsWoodenBridge)
          } else if (h.terrainMap != FourSeasonsCastle) {
            h.terrainMap = FourSeasonsRoad
          }
        }
      }
    }
  }

  def makeRoadAroundCapitals(field: FourSeasonsTerrainHexField, capital:FourSeasonsTerrainHex): Unit = {
    field.hexRing(capital, 1).foreach { h =>
      if (h.terrainMap != FourSeasonsWater && h.terrainMap != FourSeasonsRiver) {
        h.terrainMap = FourSeasonsRoad
      }
    }
  }

  def addFlowers(field: FourSeasonsTerrainHexField): Unit = {
    field.hexes.filter(_.terrainMap == FourSeasonsGrass).filter(_.mapObj.isEmpty).grouped(10).map(_.head).foreach {
      h => h.mapObj = Some(FourSeasonsFlowers)
    }
  }
}

case class WorldMap(terrain: FourSeasonsTerrainHexField, provinces: Map[FourSeasonsTerrainHex, Set[FourSeasonsTerrainHex]])

case class MapDivision[T <: Hex](capitals: Set[T], allHexes: Set[T]) {

  def voronoiDivision: Map[T, Set[T]] = {
    val map = capitals.map(c => c -> MercUtils.concurrentMutableSet[T]()).toMap
    allHexes.par.foreach { h =>
      val closestCapital = capitals.toSeq.minBy(_.distance(h))
      map(closestCapital) += h
    }
    map.transform { case (_, v) => v.toSet }
  }

  def lloydRelaxationCapitals: Set[T] = {
    voronoiDivision.par.map { case (_, hexes) =>
      val x = math.round(hexes.toList.map(_.x).sum.toDouble / hexes.size).toInt
      val y = math.round(hexes.toList.map(_.y).sum.toDouble / hexes.size).toInt
      val newCapitalCandidate = new Hex(x, y)
      hexes.minBy(_.distance(newCapitalCandidate))
    }.toSet.seq
  }
}
