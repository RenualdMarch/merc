package mr.merc.map.pathfind

import mr.merc.map.{Grid, PossibleGrid, ShortestGrid}

import scala.annotation.tailrec
import scala.collection.mutable

class PathFindingBase {
  def calculatePossible[T](grid: PossibleGrid[T], from: T, maxPrice: Int, alreadyMoved: Boolean): Map[T, Node[T]] = {
    val queue = collection.mutable.Queue[Node[T]]()
    val calculated = collection.mutable.Map[T, Node[T]]()
    queue enqueue new Node(from, 0, None)

    while (queue.nonEmpty) {
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

  private def getNeighbours[T](parent: Node[T], grid: PossibleGrid[T]): List[Node[T]] = {
    grid.neighbours(parent.t) filterNot grid.isBlocked map (n => {
      new Node(n, grid.price(parent.t, n), Some(parent))
    })
  }

  // TODO test me
  def calculatePathPrice[T](grid: Grid[T], path: List[T]): Double = {
    path zip path.tail map {
      case (from, to) => grid.price(from, to)
    } sum
  }

  def astar[T <: AnyRef](grid: ShortestGrid[T], from: T, to: T): Option[List[T]] = {
    val cameFrom = mutable.AnyRefMap[T, T]()
    val costSoFar = mutable.AnyRefMap[T, Double]()

    val ord: Ordering[(T, Double)] = new Ordering[(T, Double)] {
      override def compare(x: (T, Double), y: (T, Double)): Int = x._2.compare(y._2)
    }

    val frontier = mutable.PriorityQueue[(T, Double)]()(ord.reverse)
    frontier.enqueue((from, 0))

    cameFrom += from -> from
    costSoFar += from -> 0

    while (frontier.nonEmpty) {
      val (current, _) = frontier.dequeue()
      if (current == to) {
        return Some(expandCameFrom(from, to, cameFrom))
      }

      grid.neighbours(current).filterNot(grid.isBlocked).foreach { n =>
        val newCost = costSoFar(current) + grid.price(current, n)

        if (!costSoFar.contains(n) || newCost < costSoFar(n)) {
          costSoFar(n) = newCost
          val priority = newCost + grid.heuristic(n, to)
          frontier.enqueue((n, priority))
          cameFrom(n) = current
        }
      }
    }

    None
  }

  @tailrec
  private def expandCameFrom[T](from: T, to: T, cameFrom: mutable.Map[T, T], acc:List[T] = Nil):List[T] = {
    if (from == to) to :: acc
    else {
      val newTo = cameFrom(to)
      expandCameFrom(from, newTo, cameFrom, to :: acc)
    }
  }
}