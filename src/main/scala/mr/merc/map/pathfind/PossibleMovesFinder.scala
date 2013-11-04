package mr.merc.map.pathfind

import mr.merc.map.Grid

object PossibleMovesFinder extends PathFindingBase {
	def findPossibleMoves[T](grid:Grid[T], from:T, maxPrice:Int, alreadyMoved:Boolean):Set[T] = {
	  calculatePossible(grid, from, maxPrice, alreadyMoved).keys.toSet
	}
}