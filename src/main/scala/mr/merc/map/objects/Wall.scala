package mr.merc.map.objects

import mr.merc.map.hex._

case class Wall(hex:Hex, walls:Set[Direction])

case class Walls(walls: List[Wall] = Nil, keepWalls: List[Wall] = Nil) {

  private val infiniteHexField = new InfiniteHexField((x, y) => new Hex(x, y))

  private val fullSet = Set(N, NE, NW, S, SE, SW)

  private def direction(from:Wall, to:Wall): Direction = {
    infiniteHexField.direction(from.hex, to.hex)
  }

  def addKeepWall(wall: Wall, removeInnerWall: Boolean = true): Walls = {
    require(!walls.exists(w => w.hex == wall.hex), s"wall in hex ${wall.hex} already present")
    require(!keepWalls.exists(w => w.hex == wall.hex), s"wall in hex ${wall.hex} already present")

    val newKeepWalls = addWallToList(wall, keepWalls, removeInnerWall)
    val newWalls = removeIntersectionsFromList(wall, walls)
    Walls(newWalls, newKeepWalls)
  }

  def addWall(hex: Hex, removeInnerWall: Boolean = true): Walls = {
    addWall(Wall(hex, fullSet), removeInnerWall)
  }

  def addWall(wall:Wall, removeInnerWall: Boolean): Walls = {
    require(!walls.exists(w => w.hex == wall.hex), s"wall in hex ${wall.hex} already present")
    require(!keepWalls.exists(w => w.hex == wall.hex), s"wall in hex ${wall.hex} already present")

    val intersectionsWithKeeps = findIntersections(wall, keepWalls)

    val list = addWallToList(wall.copy(walls = wall.walls -- intersectionsWithKeeps),
      this.walls, removeInnerWall)
    this.copy(walls = list)
  }

  private def addWallToList(wall: Wall, list: List[Wall], removeInnerWall: Boolean):List[Wall] = {
    val intersections = findIntersections(wall, list)

    val newWall = wall.copy(walls = wall.walls -- intersections)
    if (removeInnerWall) {
      val newWalls = removeIntersectionsFromList(wall, list)
      newWalls :+ newWall
    } else {
      list :+ newWall
    }
  }

  private def findIntersections(wall: Wall, list: List[Wall]):Set[Direction] = {
    val neigs = findNeighbours(wall, list)
    neigs.foldLeft(Set[Direction]()){case (set, neighbour) =>
      val dir = direction(wall, neighbour)
      set + dir
    }
  }

  private def removeIntersectionsFromList(wall: Wall, list: List[Wall]):List[Wall] = {
    val neigs = findNeighbours(wall, list)
    val intersections = findIntersections(wall, neigs)

    list.map { current =>
      if (neigs.contains(current) && intersections.contains(direction(wall, current))) {
        val opposite = direction(wall, current).opposite
        current.copy(walls = current.walls.filterNot(_ == opposite))
      } else {
        current
      }
    }
  }

  private def findNeighbours(wall: Wall, list:List[Wall]): List[Wall] = {
    val neigs = infiniteHexField.neighbours(wall.hex)
    list.filter(w => neigs.exists(n => n.x == w.hex.x && n.y == w.hex.y))
  }
}