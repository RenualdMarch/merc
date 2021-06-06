package mr.merc.map.hex.view

import org.scalatest.{FunSuite, Matchers}
import mr.merc.map.hex._
import mr.merc.map.terrain._
import mr.merc.map.terrain.TerrainType._

class TerrainHexAdditiveRuleTest extends FunSuite with Matchers {
  val rule = new TerrainHexViewAdditiveRule

  test("filtering test") {
    val list1 = List(new TerrainHexViewAdditive(N, S, DesertSand, OldRoad))
    assert(rule.filterNotNeededAdditives(list1) === list1)

    val list2 = List(new TerrainHexViewAdditive(N, S, OldRoad, DesertSand))
    assert(rule.filterNotNeededAdditives(list2).isEmpty)
  }

  test("additive to elements conversion when there is element with same size") {
    val add = new TerrainHexViewAdditive(N, N, OldRoad, DesertSand)
    val elements = rule.additivesToElements(add)
    assert(elements === List(TerrainHexViewAdditiveElement(DesertSand, N, N)))
  }

  test("additive to elements conversion when two elements must be combined") {
    val add = new TerrainHexViewAdditive(NE, NW, OldRoad, DesertSand)
    val elements = rule.additivesToElements(add).toSet

    assert(elements.size == 2)
    assert(elements.exists(_.to == NW))
    assert(elements.exists(_.from == NE))
  }

  test("optimal additive") {
    val add = new TerrainHexViewAdditive(N, NW, ShallowWater, BankInside)
    val elements = rule.additivesToElements(add)

    assert(elements === List(
      TerrainHexViewAdditiveElement(BankInside,SW,NW),
      TerrainHexViewAdditiveElement(BankInside,SE,S),
      TerrainHexViewAdditiveElement(BankInside,N,NE)))
  }

  test("overlapping elements") {
    val el1 = DirectionsRange(NE, NW)
    val el2 = DirectionsRange(N, S)
    val el3 = DirectionsRange(N, N)

    assert(el1.intersects(el2))
    assert(!el1.intersects(el3))
  }

  test("sum of elements") {
    val el1 = DirectionsRange(N, N)
    val el2 = DirectionsRange(NE, NE)
    val el3 = DirectionsRange(SE, S)

    assert(el1 + el2 + el3 === DirectionsRange(N, S))
  }

  test("transform") {
    val list = List(new TerrainHexViewAdditive(NE, NE, DesertSand, GreenGrass),
      new TerrainHexViewAdditive(N, N, BasicHill, GreenGrass))
    val result = rule.transform(list)
    assert(result.size === 1)
    assert(result.contains(TerrainHexViewAdditiveElement(GreenGrass, NE, NE)))
  }

  test("single water tile") {
    val additive = TerrainHexViewAdditive(N,NW,ShallowWater,BankInside)
    val elements = rule.additivesToElements(additive)
    val sum = elements.map(e => DirectionsRange(e.from, e.to)).reduce(_ + _)
    sum shouldBe DirectionsRange(N, NW)
  }
}