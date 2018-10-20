package mr.merc.map.pathfind

import mr.merc.map.PossibleGrid

object PossibleMovesFinder extends PathFindingBase {
	def findPossibleMoves[T](grid:PossibleGrid[T], from:T, maxPrice:Int, alreadyMoved:Boolean):Set[T] = {
	  calculatePossible(grid, from, maxPrice, alreadyMoved).keys.toSet
	}
}