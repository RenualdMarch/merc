package mr.merc.map.hex

import mr.merc.map.terrain.Grass
import mr.merc.map.terrain.Sand
import mr.merc.map.terrain.Hill
import scala.collection.generic.CanBuildFrom
import mr.merc.map.terrain.TerrainType

object TerrainHexViewAdditiveRule {
  // first is drawn first
  private val orderOfTypes = List(Sand, Grass, Hill)
}

/**
 * Rule decides which elements should be drawn
 */
class TerrainHexViewAdditiveRule {
    def transform(add:Traversable[TerrainHexViewAdditive]):List[TerrainHexViewAdditiveElement]  = {
      val filtered = filterNotNeededAdditives(add)
      filtered.flatMap(additivesToElements)
    }
  
	private [hex] def filterNotNeededAdditives(add:Traversable[TerrainHexViewAdditive]):List[TerrainHexViewAdditive] = {
	  add.filter(viewAdd => {
	    if (TerrainType.helperTypesList.contains(viewAdd.hexTerrainType) || TerrainType.helperTypesList.contains(viewAdd.neighbourTerrainType)) {
	      true
	    } else {
	        val strengthOfCurrent = TerrainHexViewAdditiveRule.orderOfTypes.indexOf(viewAdd.hexTerrainType)
	        val strengthOfNeighbour = TerrainHexViewAdditiveRule.orderOfTypes.indexOf(viewAdd.neighbourTerrainType)
	        strengthOfCurrent < strengthOfNeighbour
	    }
	  }).toList
	}
	
	private [hex] def additivesToElements(add:TerrainHexViewAdditive):List[TerrainHexViewAdditiveElement] = {
	  val allElements = TerrainHexViewAdditiveElement.elementsByType(add.neighbourTerrainType).toList
      val possibleElements = allElements.filter(e => additiveContainsElement(add, e))
      additivesToElementsRec(add, Set(), possibleElements).toList	   
	}
	
	private def additivesToElementsRec(add:TerrainHexViewAdditive, acc:Set[TerrainHexViewAdditiveElement], possible:List[TerrainHexViewAdditiveElement]):Set[TerrainHexViewAdditiveElement] = {
	  if (!acc.isEmpty && !areElementsOverlapping(acc) && sumOfElementsSlices(acc) == Set((add.from, add.to))) {
	    acc
	  } else if (!acc.isEmpty && sumOfElementsSlices(acc) != Set((add.from, add.to)) && possible.isEmpty) {
	    Set()
	  } else {
		val possibleElements = possible.filter(p => !areElementsOverlapping(acc + p))
		if (possibleElements.size == 0) {
		  println("empty")
		}
	    val currentResult = additivesToElementsRec(add, acc + possibleElements.head, possibleElements.tail)
		if (!currentResult.isEmpty) {
		  currentResult
		} else {
		  additivesToElementsRec(add, acc, possible.tail)
		}
	  }	  
	}
	
	private def additiveContainsElement(add:TerrainHexViewAdditive, elem:TerrainHexViewAdditiveElement):Boolean = {
	  val addDirections = (add.from, add.to)
	  val elemDirection = (elem.from, elem.to)
	  Directions.leftSliceContainsRightSlice(addDirections, elemDirection)
	}
	
	private [hex] def areElementsOverlapping(elements:Traversable[TerrainHexViewAdditiveElement]):Boolean = {
	  elements.exists(el1 => {
	    elements.exists(el2 => el1 != el2 && Directions.overlapping((el1.from, el1.to), (el2.from, el2.to)))
	  })	  
	}
	
	private [hex] def sumOfElementsSlices(elements:Traversable[TerrainHexViewAdditiveElement]):Set[(Directions.Direction, Directions.Direction)] = {
	  val set = elements.map(el => (el.from, el.to)).toSet
	  Directions.unite(set)
	}
}