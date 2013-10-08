package mr.merc.map.hex.view

import org.scalatest.FunSuite
import mr.merc.map.terrain.Grass
import mr.merc.map.terrain.Sand
import mr.merc.map.terrain.Hill
import mr.merc.map.hex._

class TerrainHexAdditiveRuleTest extends FunSuite{
	val rule = new TerrainHexViewAdditiveRule
	
	test("filtering test") {
	  val list1 = List(new TerrainHexViewAdditive(N, S, Sand, Grass));
	  
	  assert(rule.filterNotNeededAdditives(list1) === list1)
	  
	  val list2 = List(new TerrainHexViewAdditive(N, S, Hill, Grass));
	  assert(rule.filterNotNeededAdditives(list2).isEmpty)
	}
	
	test("additive to elements conversion when there is element with same size") {
	  val add = new TerrainHexViewAdditive(N, N, Sand, Grass)
	  val elements = rule.additivesToElements(add)
	  
	  assert(elements.size === 1)
	  val element = elements(0)
	  assert(element === TerrainHexViewAdditiveElement(Grass, N, N))
	}
	
	test("additive to elements conversion when three elements must be combined") {
	  val add = new TerrainHexViewAdditive(NE, SW, Sand, Grass)
	  val elements = rule.additivesToElements(add)
	  
	  assert(elements.size === 3)
	  assert(elements.contains(TerrainHexViewAdditiveElement(Grass, NE, NE)))
	  assert(elements.contains(TerrainHexViewAdditiveElement(Grass, S, SW)))
	  assert(elements.contains(TerrainHexViewAdditiveElement(Grass, SE, SE)))
	}
	
	test("additive to elements conversion when two elements must be combined") {
	  val add = new TerrainHexViewAdditive(N, S, Sand, Grass)
	  val elements = rule.additivesToElements(add)
	  
	  assert(elements.size === 2)
	  assert(elements.contains(TerrainHexViewAdditiveElement(Grass, N, SE)))
	  assert(elements.contains(TerrainHexViewAdditiveElement(Grass, S, S)))

	}
	
	test("overlapping elements") {
	  val el1 = TerrainHexViewAdditiveElement(Grass, NE, NW)
	  val el2 = TerrainHexViewAdditiveElement(Grass, N, S)
	  val el3 = TerrainHexViewAdditiveElement(Grass, N, N)
	  
	  assert(rule.areElementsOverlapping(List(el1, el2)))
	  assert(!rule.areElementsOverlapping(List(el2)))
	  assert(!rule.areElementsOverlapping(List(el1, el3)))
	}
	
	test("sum of elements") {
	  val el1 = TerrainHexViewAdditiveElement(Grass, N, N)
	  val el2 = TerrainHexViewAdditiveElement(Grass, NE, NE)
	  val el3 = TerrainHexViewAdditiveElement(Grass, SE, S)
	  
	  assert(rule.sumOfElementsSlices(List(el1, el2, el3)) === Set((N, S)))
	}
	
	test("elements by terrain type") {
	  val grass = TerrainHexViewAdditiveElement.elementsByType(Grass)
	  assert(grass.size === 8)
	  val sand = TerrainHexViewAdditiveElement.elementsByType(Sand)
	  assert(sand.size === 6)
	}
	
	test("transform") {
	  val list = List(new TerrainHexViewAdditive(NE, NE, Sand, Grass), 
	      new TerrainHexViewAdditive(N, N, Hill, Grass));
	  val result = rule.transform(list)
	  assert(result.size === 1)
	  assert(result.contains(TerrainHexViewAdditiveElement(Grass, NE, NE)))
	}
}