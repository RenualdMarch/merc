package mr.merc.map.generator

import mr.merc.economics.WorldGenerator
import mr.merc.map.ShortestGrid
import mr.merc.map.hex._
import mr.merc.map.objects.{Flowers, WoodenBridge}
import mr.merc.map.pathfind.PathFinder
import mr.merc.map.terrain._

import scala.collection.mutable
import scala.math.{abs, max, pow}
import scala.util.Random

object WorldMapGenerator {
  val landPercentage = 0.6

  def generateWorldMap(width: Int, height: Int, provinces: Int): WorldMap = {
    val terrainNoise = Noise(5).add(0.5, Noise(10)).add(0.25, Noise(20)).applyFunction { case ((x, y), n) =>
      val distanceFromCenter = 2 * max(abs(x - 0.5), abs(y - 0.5))
      n + 0.6 - 1.6 * pow(distanceFromCenter, 2)
    }

    val biomeNoise = Noise(10).add(0.5, Noise(40)).add(0.25, Noise(80))

    def biome(x: Int, y: Int): TerrainType = {
      biomeNoise(x, width, y, height) match {
        //case n if n < biomeNoise.percentageBelow(0.2) => DesertSand
        case n if n < biomeNoise.percentageBelow(0.6) => GreenGrass
        case n if n < biomeNoise.percentageBelow(0.85) => DecForest
        case n if n < biomeNoise.percentageBelow(0.98) => BasicHill
        case _ => BasicMountain
      }
    }

    def f(x: Int, y: Int): TerrainHex = {
      val n = terrainNoise(x, width, y, height)
      if (n > terrainNoise.percentageBelow(1 - landPercentage)) new TerrainHex(x, y, biome(x, y))
      else new TerrainHex(x, y, ShallowWater)
    }

    val terrainField = new TerrainHexField(width, height, f)

    val provincesMap = divideIntoProvinces(terrainField, provinces)
    provincesMap.keys.foreach { h =>
      terrainField.hex(h.x, h.y).terrain = Castle
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

  def divideIntoProvinces(field: HexField[TerrainHex], provinces: Int): Map[TerrainHex, Set[TerrainHex]] = {
    val totalHexes = field.hexes.filterNot(_.terrain.is(WaterKind))
    val firstCapitals = Random.shuffle(totalHexes).take(provinces)
    val division = 0.until(10).foldLeft(MapDivision(firstCapitals.toSet, totalHexes.toSet)) { case (div, _) =>
      val newCapitals = div.lloydRelaxationCapitals
      MapDivision(newCapitals, div.allHexes)
    }

    MapDivision(division.capitals, field.hexes.toSet).voronoiDivision
  }

  def addRivers(field: TerrainHexField): Unit = {
    val riversCount = (field.width + field.height) / 3
    val hexesPerCurve = 4

    val initialRivers = (0 until riversCount).flatMap { _ =>
      val x = Random.nextInt(field.width)
      val y = Random.nextInt(field.height - 1)
      val from = field.hex(x, y)

      field.findClosest(from, _.terrain.is(WaterKind)).flatMap { target =>
        field.findPath(from, target, h => h.terrain == Castle ||
          field.neighbours(h).exists(_.terrain == Castle))
      }
    }
    val blocks = initialRivers.flatMap { river =>
      river.drop(hexesPerCurve - 1).grouped(hexesPerCurve).map(_.head)
    }.toSet

    val initialRiversHexes = initialRivers.flatten.toSet -- blocks

    val pathGrid = new ShortestGrid[TerrainHex] {
      override def heuristic(from: TerrainHex, to: TerrainHex): Double = {
        math.abs(from.x - to.x) + math.abs(from.y - to.y)
      }

      override def isBlocked(t: TerrainHex): Boolean = t.terrain == Castle || neighbours(t).exists(_.terrain == Castle) || blocks.contains(t)

      override def price(from: TerrainHex, to: TerrainHex): Double = if (initialRiversHexes.contains(to)) 0.1 else 1

      override def neighbours(t: TerrainHex): List[TerrainHex] = field.neighbours(t)
    }

    initialRivers.foreach { river =>
      val head = river.head
      val last = river.last
      PathFinder.findPath(pathGrid, head, last).toList.flatten.foreach { h =>
        h.terrain = ShallowWater
      }
    }
  }

  def connectCitiesByRoads(field: TerrainHexField, provincesMap: Map[TerrainHex, Set[TerrainHex]]): Unit = {
    val connectivityMap = WorldGenerator.buildConnectivityMap(field, provincesMap)
    val connections = connectivityMap.flatMap { case (capital, neigs) =>
      neigs.map(h => Set(capital, h))
    }.toSet

    connections.map(_.toList).foreach { case List(from, to) =>
      val grid = new ShortestGrid[TerrainHex] {
        override def heuristic(from: TerrainHex, to: TerrainHex): Double = price(from, to)

        override def isBlocked(t: TerrainHex): Boolean = !provincesMap(from).contains(t) && !provincesMap(to).contains(t)

        override def price(from: TerrainHex, to: TerrainHex): Double =
          if (to.terrain.is(RoadKind)) 0.5
          else if (to.mapObj.contains(WoodenBridge)) 0.7
          else if (to.terrain.is(WaterKind)) 3
          else 1

        override def neighbours(t: TerrainHex): List[TerrainHex] = field.neighbours(t)
      }

      PathFinder.findPath(grid, from, to).foreach { path =>
        path.foreach { h =>
          if (h.terrain.is(WaterKind)) {
            h.mapObj = Some(WoodenBridge)
          } else if (h.terrain != Castle) {
            h.terrain = GrassyRoad
          }
        }
      }
    }
  }

  def makeRoadAroundCapitals(field: TerrainHexField, capital:TerrainHex): Unit = {
    field.hexRing(capital, 1).foreach { h =>
      if (h.terrain.isNot(WaterKind)) {
        h.terrain = GrassyRoad
      }
    }
  }

  def addFlowers(field: TerrainHexField): Unit = {
    field.hexes.filter(_.terrain.is(GrassKind)).filter(_.mapObj.isEmpty).grouped(10).map(_.head).foreach {
      h => h.mapObj = Some(Flowers)
    }
  }
}

case class WorldMap(terrain: TerrainHexField, provinces: Map[TerrainHex, Set[TerrainHex]])

case class MapDivision[T <: Hex](capitals: Set[T], allHexes: Set[T]) {

  def voronoiDivision: Map[T, Set[T]] = {
    val map = capitals.map(c => c -> mutable.Set[T]()).toMap
    allHexes.foreach { h =>
      val closestCapital = capitals.toSeq.minBy(_.distance(h))
      map(closestCapital) += h
    }
    map.transform { case (_, v) => v.toSet }
  }

  def lloydRelaxationCapitals: Set[T] = {
    voronoiDivision.map { case (_, hexes) =>
      val x = math.round(hexes.toList.map(_.x).sum.toDouble / hexes.size).toInt
      val y = math.round(hexes.toList.map(_.y).sum.toDouble / hexes.size).toInt
      val newCapitalCandidate = new Hex(x, y)
      hexes.minBy(_.distance(newCapitalCandidate))
    } toSet
  }
}
