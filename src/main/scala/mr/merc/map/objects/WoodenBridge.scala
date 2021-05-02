package mr.merc.map.objects

import mr.merc.map.hex.Direction
import mr.merc.map.hex.TerrainHex
import mr.merc.image.MImage
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.objects.view.MapObjectView
import mr.merc.map.terrain.TerrainKind._

object WoodenBridge extends MapObject("woodenBridge") {
  private[objects] def imagePath(fileName: String) = "/images/terrain/" + name + "/" + fileName + ".png"

  private val endsMap = Direction.list.map(dir => (dir, "end-" + dir.toString().toLowerCase())).toMap

  private val possibleTwoSides:List[TwoSides] = for {
    from <- Direction.list
    to <- Direction.list if Option(getClass.getResource(TwoSides(from, to).path)).nonEmpty
  } yield TwoSides(from, to)

  private val possibleThreeSides:List[ThreeSides] = for {
    dir1 <- Direction.list
    dir2 <- Direction.list
    dir3 <- Direction.list if Option(getClass.getResource(ThreeSides(dir1, dir2, dir3).path)).nonEmpty
  } yield ThreeSides(dir1, dir2, dir3)

  private val possibleCenters = possibleTwoSides ++ possibleThreeSides

  override def view: MapObjectView = new MapObjectView {
    override def images(hex: TerrainHex, field: TerrainHexField): List[MImage] = {
      val neighbours = field.neighboursWithDirections(hex.x, hex.y)
      hex.mapObj match {
        case Some(WoodenBridge) => List(imagesForWoodenBridge(neighbours))
        case _ => imagesForWoodenBridgeNeighbour(hex, field)
      }
    }
  }

  private def imagesForWoodenBridge(neighbours: Map[Direction, TerrainHex]):MImage = {
    val variant = selectVariant(neighbours)
    MImage(variant.path)
  }

  private def selectVariant(neighbours: Map[Direction, TerrainHex]): WoodenBridgeVariant = {
    possibleCenters.maxBy(_.points(neighbours))
  }

  private def pointsForNeighbour(neig: TerrainHex): Int = {
    if (neig.mapObj.contains(WoodenBridge)) 20
    else if (neig.terrain.is(RoadKind) && neig.mapObj.nonEmpty) 4
    else if (neig.terrain.is(RoadKind)) 5
    else if (neig.terrain.is(MountainKind)) -5
    else if (neig.terrain.is(WaterKind)) -2
    else -1
  }

  private def imagesForWoodenBridgeNeighbour(hex: TerrainHex, field: TerrainHexField): List[MImage] = {
    if (hex.mapObj.contains(WoodenBridge)) {
      return Nil
    }

    val bridges = field.neighboursWithDirections(hex.x, hex.y).
      filter(pair => pair._2.mapObj.contains(WoodenBridge))
    val directions = bridges.flatMap { case (direction, neig) =>
      val variant = selectVariant(field.neighboursWithDirections(neig))
      if (variant.contains(direction.opposite)) {
        Some(direction)
      } else {
        None
      }
    }

    directions.flatMap(endsMap.get).map(n => MImage(imagePath(n))).toList
  }

  private sealed trait WoodenBridgeVariant {
    def path: String
    def points(neighbours: Map[Direction, TerrainHex]): Int
    def contains(dir: Direction): Boolean
  }

  private case class TwoSides(from: Direction, to: Direction) extends WoodenBridgeVariant {
    val path: String = imagePath(from.toString().toLowerCase() + "-" + to.toString().toLowerCase())
    def points(neighbours: Map[Direction, TerrainHex]): Int =
      List(neighbours.get(from), neighbours.get(to)).flatten.map(pointsForNeighbour).sum + bonusPoints
    def contains(dir: Direction): Boolean = Set(from, to).contains(dir)

    private def bonusPoints: Int = if(from == to.opposite) 1 else 0
  }

  private case class ThreeSides(dir1: Direction, dir2: Direction, dir3: Direction) extends WoodenBridgeVariant {
    val path: String = imagePath(dir1.toString().toLowerCase() + "-" + dir2.toString().toLowerCase() + "-" + dir3.toString().toLowerCase())
    def points(neighbours: Map[Direction, TerrainHex]): Int = List(neighbours.get(dir1),neighbours.get(dir2),neighbours.get(dir3)).flatten.map(pointsForNeighbour).sum
    def contains(dir: Direction): Boolean = Set(dir1, dir2, dir3).contains(dir)
  }
}