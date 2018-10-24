package mr.merc.map.hex.view

import org.scalatest.FunSuite
import mr.merc.map.terrain._
import mr.merc.map.hex.Direction
import mr.merc.map.hex._
import mr.merc.map.hex.TerrainHex
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.view.SoldiersDrawer

class TerrainHexAdditiveTest extends FunSuite {

  def exist(tr: Traversable[TerrainHexViewAdditive], from: Direction, to: Direction, hexType: TerrainType, tp: TerrainType) {
    assert(tr.exists(ad => ad.from == from && ad.to == to && ad.neighbourTerrainType == tp && hexType == ad.hexTerrainType))
  }

  test("case with different single terrains") {
    val deter = new TypeDeterminator(Map((0, 1) -> DesertSand, (2, 1) -> Empty, (0, 2) -> Empty, (1, 2) -> DesertSand))
    val field = new TerrainHexField(5, 5, deter)
    val view = new TerrainHexFieldView(field, new SoldiersDrawer(), 1.0)
    val additives = TerrainHexViewAdditive.extractAdditives(view.hex(1, 1))
    assert(additives.size === 2)
    exist(additives, NW, NW, GreenGrass, DesertSand)
    exist(additives, S, S, GreenGrass, DesertSand)
  }

  test("case with single and multiple terrains") {
    val deter = new TypeDeterminator(Map((0, 1) -> DesertSand, (1, 0) -> DesertSand, (2, 1) -> DesertSand, (1, 2) -> DesertSand), OldRoad)
    val field = new TerrainHexField(5, 5, deter)
    val view = new TerrainHexFieldView(field, new SoldiersDrawer(), 1.0)
    val additives = TerrainHexViewAdditive.extractAdditives(view.hex(1, 1)).toSet
    assert(additives === Set(
      TerrainHexViewAdditive(N,NE,OldRoad,DesertSand),
      TerrainHexViewAdditive(NW,NW,OldRoad,DesertSand),
      TerrainHexViewAdditive(S, S, OldRoad, DesertSand)
    ))
  }

  test("case on the edge") {
    val deter = new TypeDeterminator(Map((1, 0) -> DesertSand, (2, 1) -> DesertSand))
    val field = new TerrainHexField(5, 5, deter)
    val view = new TerrainHexFieldView(field, new SoldiersDrawer(), 1.0)
    val additives = TerrainHexViewAdditive.extractAdditives(view.hex(2, 0))
    assert(additives.size === 1)
    exist(additives, S, SW, GreenGrass, DesertSand)
  }

  test("normalizing when covering full circle") {
    val add1 = new TerrainHexViewAdditive(N, NE, GreenGrass, DesertSand)
    val add2 = new TerrainHexViewAdditive(NW, N, GreenGrass, DesertSand)
    val add3 = new TerrainHexViewAdditive(SE, N, GreenGrass, DesertSand)
    val add4 = new TerrainHexViewAdditive(S, SE, GreenGrass, DesertSand)

    assert(add1.from === N)
    assert(add1.to === NE)

    assert(add2.from === NW)
    assert(add2.to === N)

    assert(add3.from === SE)
    assert(add3.to === N)

    assert(add4.from === S)
    assert(add4.to === SE)
  }

  test("case with water") {
    val deter = new TypeDeterminator(Map((1, 0) -> DesertSand, (2, 1) -> ShallowWater, (2, 2) -> ShallowWater), OldRoad)
    val field = new TerrainHexField(3, 5, deter)
    val view = new TerrainHexFieldView(field, new SoldiersDrawer(), 1.0)

    val additives = TerrainHexViewAdditive.extractAdditives(view.hex(1, 1))
    assert(additives.size === 2)
    exist(additives, N, N, OldRoad, DesertSand)
    exist(additives, NE, SE, OldRoad, BankOutside)

    val additivesOnWater = TerrainHexViewAdditive.extractAdditives(view.hex(2, 1))
    assert(additivesOnWater.toSet === Set(TerrainHexViewAdditive(SW,NW,ShallowWater,BankInside),
      TerrainHexViewAdditive(N,N,ShallowWater,BankInside)))
  }

  test("road and grass") {
    val deter = new TypeDeterminator(Map((1, 0) -> CleanRoad, (1, 1) -> CleanRoad,
      (1, 2) -> CleanRoad, (1, 3) -> CleanRoad), GreenGrass)
    val field = new TerrainHexField(5, 5, deter)
    val view = new TerrainHexFieldView(field, new SoldiersDrawer(), 1.0)
    val additives = TerrainHexViewAdditive.extractAdditives(view.hex(1, 1))

    assert(additives.toSet ===
      Set(TerrainHexViewAdditive(NE,SE,CleanRoad,GreenGrass),
        TerrainHexViewAdditive(SW,NW,CleanRoad,GreenGrass)))
  }
}
