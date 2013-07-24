package mr.merc.map.hex

import mr.merc.map.terrain.Grass
import mr.merc.map.terrain.Sand
import mr.merc.map.terrain.Hill

object TerrainHexViewAdditiveRule {
  // first is drawn first
  private val orderOfTypes = List(Grass, Sand, Hill)
}

/**
 * Rule decides which elements should be drawn
 */
class TerrainHexViewAdditiveRule {
	private [hex] def filterNotNeededAdditives(add:Traversable[TerrainHexViewAdditive]):Traversable[TerrainHexViewAdditive] = {
	  ???
	}
	
	private [hex] def additivesToElements(add:TerrainHexViewAdditive):List[TerrainHexViewAdditiveElement] = {
	  ???
	}
}