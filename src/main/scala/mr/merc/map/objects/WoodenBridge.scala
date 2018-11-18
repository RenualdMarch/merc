package mr.merc.map.objects

import mr.merc.map.hex.Direction
import mr.merc.map.hex.TerrainHex
import mr.merc.image.MImage

import mr.merc.map.hex.TerrainHexField
import mr.merc.map.terrain.{MountainKind, RoadKind, WaterKind}

object WoodenBridge extends MapObject("woodenBridge") {
  private[objects] def imagePath(fileName: String) = "/images/terrain/" + name + "/" + fileName + ".png"

  private val endsMap = Direction.list.map(dir => (dir, "end-" + dir.toString().toLowerCase())).toMap

  private val possibleCenters:List[(Direction, Direction)] = for {
    from <- Direction.list
    to <- Direction.list if from != to && Option(getClass.getResource(imagePath(centerToPath(from, to)))).nonEmpty
  } yield (from, to)

  private def centerToPath(from: Direction, to: Direction): String =
    from.toString().toLowerCase() + "-" + to.toString().toLowerCase()

  private def centerToPath(pair:(Direction, Direction)): String = centerToPath(pair._1, pair._2)

  override def images(hex: TerrainHex, field: TerrainHexField): List[MImage] = {
    val neighbours = field.neighboursWithDirections(hex.x, hex.y)
    hex.mapObj match {
      case Some(WoodenBridge) => List(imagesForWoodenBridge(neighbours))
      case _ => imagesForWoodenBridgeNeighbour(hex, field)
    }
  }

  private def imagesForWoodenBridge(neighbours: Map[Direction, TerrainHex]):MImage = {
    val part = centerToPath(directionsForWoodenBridge(neighbours))
    MImage(imagePath(part))
  }

  private def directionsForWoodenBridge(neighbours: Map[Direction, TerrainHex]): (Direction, Direction) = {
    val list = possibleCenters.map {
      case (from, to) => (from, to) -> pointsForBridge(neighbours, from, to)
    }
    list.maxBy(_._2)._1
  }

  private def pointsForBridge(neighbours: Map[Direction, TerrainHex], from: Direction, to: Direction): Int = {
    List(neighbours.get(from), neighbours.get(to)).flatten.map(pointsForNeighbour).sum
  }

  private def pointsForNeighbour(neig: TerrainHex): Int = {
    if (neig.mapObj.contains(WoodenBridge)) 20
    else if (neig.terrain.is(RoadKind) && neig.mapObj.nonEmpty) 4
    else if (neig.terrain.is(RoadKind)) 5
    else if (neig.terrain.is(MountainKind)) 1
    else if (neig.terrain.is(WaterKind)) -2
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