package mr.merc.map.pathfind

import mr.merc.map.{Grid, PossibleGrid, ShortestGrid, UniversalGrid}


object PathFinder extends PathFindingBase {
  def findPath[T <: AnyRef](grid: ShortestGrid[T], from: T, to: T): Option[List[T]] = {
    grid match {
      case g: UniversalGrid[T] =>
        if (g.cellWhereItIsForbiddenToStop(to)) {
          None
        } else {
          val adapterGrid = new ShortestGrid[T] {
            override def heuristic(from: T, to: T): Double = g.heuristic(from, to)

            override def isBlocked(t: T): Boolean = g.isBlocked(t) ||
              (g.cellWhereMovementMustBeStopped(t) && t != to)

            override def price(from: T, to: T): Double = g.price(from, to)

            override def neighbours(t: T): List[T] = g.neighbours(t)
          }
          astar(adapterGrid, from, to)
        }
      case g: ShortestGrid[T] => astar(g, from, to)
    }
  }

  def findOptimalPath[T](grid: PossibleGrid[T], from: T, to: T, maxPrice: Int = Int.MaxValue, alreadyMoved: Boolean = false): Option[List[T]] = {
    if (grid.cellWhereItIsForbiddenToStop(to)) {
      return None
    }

    val result = calculatePossible(grid, from, maxPrice, alreadyMoved)
    result.get(to).map(_.path.map(_.t))
  }

  def findPossiblePaths[T](grid: PossibleGrid[T], from: T, maxPrice: Int = Int.MaxValue, alreadyMoved: Boolean = false): Map[T, List[T]] = {
    val result = calculatePossible(grid, from, maxPrice, alreadyMoved)
    result.transform { case (_, node) =>
      node.path.map(_.t)
    }
  }
}
	


