package mr.merc.map.objects

import mr.merc.map.hex.Direction
import mr.merc.map.hex.TerrainHex
import mr.merc.image.MImage

import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex._
import mr.merc.map.terrain.{MountainKind, RoadKind, WaterKind}

object WoodenBridge extends MapObject("woodenBridge") {
  private[objects] def imagePath(fileName: String) = "/images/terrain/" + name + "/" + fileName + ".png"

  private val endsMap = Direction.list.map(dir => (dir, "end-" + dir.toString().toLowerCase())).toMap

  private val selectionPriority = List((N, S), (NE, SW), (SE, NW), (NE, S), (N, SE), (SE, SW), (S, NW), (SW, N))
  private val centersMap = selectionPriority.map(pair => (pair, pair._1.toString().toLowerCase() + "-" +
      pair._2.toString().toLowerCase())).toMap


  override def images(hex: TerrainHex, field: TerrainHexField): List[MImage] = {
    val neighbours = field.neighboursWithDirections(hex.x, hex.y)
    hex.mapObj match {
      case Some(WoodenBridge) => List(imagesForWoodenBridge(neighbours))
      case _ => imagesForWoodenBridgeNeighbour(hex, field)
    }
  }

  private def imagesForWoodenBridge(neighbours: Map[Direction, TerrainHex]):MImage = {
    val part = centersMap(directionsForWoodenBridge(neighbours))
    MImage(imagePath(part))
  }

  private def directionsForWoodenBridge(neighbours: Map[Direction, TerrainHex]): (Direction, Direction) = {
    val list = selectionPriority.map {
      case (from, to) => (from, to) -> pointsForBridge(neighbours, from, to)
    }
    list.maxBy(_._2)._1
  }

  private def pointsForBridge(neighbours: Map[Direction, TerrainHex], from: Direction, to: Direction): Int = {
    List(neighbours.get(from), neighbours.get(to)).flatten.map(pointsForNeighbour).sum
  }

  private def pointsForNeighbour(neig: TerrainHex): Int = {
    if (neig.mapObj.contains(WoodenBridge)) 20
    else if (neig.terrain.is(RoadKind)) 5
    else if (neig.terrain.is(MountainKind)) 1
    else if (neig.terrain.is(WaterKind)) 0
    else 2
  }

  private def imagesForWoodenBridgeNeighbour(hex: TerrainHex, field: TerrainHexField): List[MImage] = {
    if (hex.mapObj.contains(WoodenBridge)) {
      return Nil
    }

    val bridges = field.neighboursWithDirections(hex.x, hex.y).
      filter(pair => pair._2.mapObj.contains(WoodenBridge))
    val directions = bridges.flatMap { case (direction, neig) =>
      val (from, to) = directionsForWoodenBridge(field.neighboursWithDirections(neig))
      if (from == direction.opposite || to == direction.opposite) {
        Some(direction)
      } else {
        None
      }
    }

    directions.flatMap(endsMap.get).map(n => MImage(imagePath(n))).toList
  }
}