package mr.merc.map.pathfind

import mr.merc.map.Grid

class PathFindingBase {
  def calculatePossible[T](grid: Grid[T], from: T, maxPrice: Int, alreadyMoved: Boolean): Map[T, Node[T]] = {
    val queue = collection.mutable.Queue[Node[T]]()
    val calculated = collection.mutable.Map[T, Node[T]]()
    queue enqueue new Node(from, 0, None)

    while (!queue.isEmpty) {
      val current = queue.dequeue()
      var addNeigboursToQueue = false
      if (current.pathPrice <= maxPrice) {
        if (calculated.contains(current.t)) {
          val earlierValue = calculated(current.t)
          if (earlierValue.pathPrice > current.pathPrice) {
            calculated.remove(earlierValue.t)
            calculated += (current.t -> current)
            addNeigboursToQueue = true
          }
        } else {
          calculated += (current.t -> current)
          addNeigboursToQueue = true
        }
      }

      val stopHere = if (from == current.t) {
        alreadyMoved && grid.cellWhereMovementMustBeStopped(current.t)
      } else {
        grid.cellWhereMovementMustBeStopped(current.t)
      }

      addNeigboursToQueue &&= !stopHere

      if (addNeigboursToQueue) {
        queue ++= getNeighbours(current, grid)
      }
    }

    calculated toMap
  }

  private def getNeighbours[T](parent: Node[T], grid: Grid[T]): Set[Node[T]] = {
    grid.neighbours(parent.t) filterNot (grid.isBlocked) map (n => {
      new Node(n, grid.price(parent.t, n), Some(parent))
    })
  }

  // TODO test me
  def calculatePathPrice[T](grid: Grid[T], path: List[T]): Double = {
    path zip path.tail map {
      case (from, to) => grid.price(from, to)
    } sum
  }
}