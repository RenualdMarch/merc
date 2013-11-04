package mr.merc.map.pathfind

import mr.merc.map.Grid
import scala.collection.mutable.ArrayBuffer

object PathFinder extends PathFindingBase {
  def findPath[T](grid:Grid[T], from:T, to:T, maxPrice:Int, alreadyMoved:Boolean = false):Option[List[T]] = {
	if (grid.cellWhereItIsForbiddenToStop(to)) {
	  return None
	}
	
	val result = calculatePossible(grid, from, maxPrice, alreadyMoved)
	result.get(to).map(_.path.map(_.t))
  }
}
	


