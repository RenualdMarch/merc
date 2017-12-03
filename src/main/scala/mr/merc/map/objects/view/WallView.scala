package mr.merc.map.objects.view

import mr.merc.image.MImage
import mr.merc.map.hex._
import mr.merc.map.objects.Walls

class WallView(walls:Walls) {
  def wallImages():Map[Hex, List[WallImage]] = {
    simpleListToWallImages()
    // TODO add keep images
  }

  private def simpleListToWallImages():Map[Hex, List[WallImage]] = {
    walls.walls.map { wall =>
      val hex = wall.hex
      val directions = wall.walls
      val list = directions.toList.sortBy(orderDirections).flatMap { direction =>
        direction match {
          case N => List(WallImage(JustTop, Concave, Simple, Left), WallImage(JustTop, Concave, Simple, Right))
          case NE => List(WallImage(Top, Concave, Simple, Right))
          case NW => List(WallImage(Top, Concave, Simple, Left))
          case S => List(WallImage(JustBottom, Convex, Simple, Left), WallImage(JustBottom, Convex, Simple, Right))
          case SE => List(WallImage(Middle, Convex, Simple, Right))
          case SW => List(WallImage(Middle, Convex, Simple, Left))
        }
      }
      hex -> list
    } toMap
  }

  private def orderDirections(direction: Direction): Int = {
    direction match {
      case N => 1
      case NE => 2
      case NW => 2
      case S => 6
      case SE => 4
      case SW => 4
    }
  }
}

sealed abstract class WallPart
object JustBottom extends WallPart
object JustTop extends WallPart
object Middle extends WallPart
object Top extends WallPart

sealed abstract class WallOrientation
object Concave extends WallOrientation
object Convex extends WallOrientation

sealed abstract class WallStructure
object Simple extends WallStructure
object Keep extends WallStructure

sealed abstract class Side
object Left extends Side
object Right extends Side

case class WallImage(wallPart: WallPart, wallOrientation: WallOrientation, wallStructure: WallStructure, side: Side) {
  def image:MImage = {
    MImage(imagePath, corrections._1, corrections._2)
  }

  private def corrections:(Int, Int) = {
    (wallPart, side) match {
      case (JustBottom, Left) => (-54, -36)
      case (JustBottom, Right) => (0, 0)
      case (JustTop, Left) => (-54, -36 -72)
      case (JustTop, Right) => (0, -72)
      case (Middle, Right) => (0, -72)
      case (Middle, Left) => (-54, -36)
      case (Top, Left) => (-54, -108)
      case (Top, Right) => (0, -72)
      case _ => (0, 0)
    }
  }

  private def imagePath:String = {
    val prefix = "/images/terrain/walls/"
    val structure = wallStructure match {
      case Simple => "castle"
      case Keep => "keep"
    }

    val orientation = wallOrientation match {
      case Concave => "concave"
      case Convex => "convex"
    }

    val suffix = (wallPart, side) match {
      case (Middle, Left) | (Top, Right)=> "l"
      case (Middle, Right) | (Top, Left) => "r"
      case (JustBottom, Left) | (JustTop, Left)=> "bl"
      case (JustBottom, Right) | (JustTop, Right) => "br"
    }

    s"$prefix$structure-$orientation-$suffix.png"
  }
}