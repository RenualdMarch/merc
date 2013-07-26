package mr.merc.map.hex

import org.scalatest.FunSuite
import mr.merc.map.terrain.Grass
import mr.merc.map.terrain.Sand
import mr.merc.map.terrain.Hill

class TerrainHexAdditiveRuleTest extends FunSuite{
	val rule = new TerrainHexViewAdditiveRule
	
	test("filtering test") {
	  val list1 = List(new TerrainHexViewAdditive(Directions.N, Directions.S, Sand, Grass));
	  
	  assert(rule.filterNotNeededAdditives(list1) === list1)
	  
	  val list2 = List(new TerrainHexViewAdditive(Directions.N, Directions.S, Hill, Grass));
	  assert(rule.filterNotNeededAdditives(list2).isEmpty)
	}
	
	test("additive to elements conversion when there is element with same size") {
	  val add = new TerrainHexViewAdditive(Directions.N, Directions.N, Sand, Grass)
	  val elements = rule.additivesToElements(add)
	  
	  assert(elements.size === 1)
	  val element = elements(0)
	  assert(element === TerrainHexViewAdditiveElement(Grass, Directions.N, Directions.N))
	}
	
	test("additive to elements conversion when three elements must be combined") {
	  val add = new TerrainHexViewAdditive(Directions.NE, Directions.SW, Sand, Grass)
	  val elements = rule.additivesToElements(add)
	  
	  assert(elements.size === 3)
	  assert(elements.contains(TerrainHexViewAdditiveElement(Grass, Directions.NE, Directions.NE)))
	  assert(elements.contains(TerrainHexViewAdditiveElement(Grass, Directions.S, Directions.SW)))
	  assert(elements.contains(TerrainHexViewAdditiveElement(Grass, Directions.SE, Directions.SE)))
	}
	
	test("additive to elements conversion when two elements must be combined") {
	  val add = new TerrainHexViewAdditive(Directions.N, Directions.S, Sand, Grass)
	  val elements = rule.additivesToElements(add)
	  
	  assert(elements.size === 2)
	  assert(elements.contains(TerrainHexViewAdditiveElement(Grass, Directions.N, Directions.SE)))
	  assert(elements.contains(TerrainHexViewAdditiveElement(Grass, Directions.S, Directions.S)))

	}
	
	test("overlapping elements") {
	  val el1 = TerrainHexViewAdditiveElement(Grass, Directions.NE, Directions.NW)
	  val el2 = TerrainHexViewAdditiveElement(Grass, Directions.N, Directions.S)
	  val el3 = TerrainHexViewAdditiveElement(Grass, Directions.N, Directions.N)
	  
	  assert(rule.areElementsOverlapping(List(el1, el2)))
	  assert(!rule.areElementsOverlapping(List(el2)))
	  assert(!rule.areElementsOverlapping(List(el1, el3)))
	}
	
	test("sum of elements") {
	  val el1 = TerrainHexViewAdditiveElement(Grass, Directions.N, Directions.N)
	  val el2 = TerrainHexViewAdditiveElement(Grass, Directions.NE, Directions.NE)
	  val el3 = TerrainHexViewAdditiveElement(Grass, Directions.SE, Directions.S)
	  
	  assert(rule.sumOfElementsSlices(List(el1, el2, el3)) === Set((Directions.N, Directions.S)))
	}
	
	test("elements by terrain type") {
	  val grass = TerrainHexViewAdditiveElement.elementsByType(Grass)
	  assert(grass.size === 8)
	  val sand = TerrainHexViewAdditiveElement.elementsByType(Sand)
	  assert(sand.size === 6)
	}
	
	test("transform") {
	  val list = List(new TerrainHexViewAdditive(Directions.NE, Directions.NE, Sand, Grass), 
	      new TerrainHexViewAdditive(Directions.N, Directions.N, Hill, Grass));
	  val result = rule.transform(list)
	  assert(result.size === 1)
	  assert(result.contains(TerrainHexViewAdditiveElement(Grass, Directions.NE, Directions.NE)))
	}
}