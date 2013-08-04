package mr.merc.map.hex

import org.scalatest.FunSuite
import mr.merc.map.terrain.TerrainType
import mr.merc.map.terrain.Grass
import mr.merc.map.terrain.Hill
import mr.merc.map.terrain.Sand
import mr.merc.map.terrain.Water
import mr.merc.map.terrain.BankOutside
import mr.merc.map.terrain.BankInside
import mr.merc.map.terrain.Mountain

class TerrainHexAdditiveTest extends FunSuite {
  
  def exist(tr:Traversable[TerrainHexViewAdditive], from:Directions.Direction, to:Directions.Direction, hexType:TerrainType, tp:TerrainType) {
    assert(tr.exists(ad => ad.from == from && ad.to == to && ad.neighbourTerrainType == tp && hexType == ad.hexTerrainType))
  }
  
  test("case with different single terrains") {
	  val deter = new TypeDeterminator(Map(((0, 1) -> Sand), ((2, 1) -> Hill), ((0, 2) -> Mountain), ((1, 2) -> Sand)))
	  val field = new TerrainHexField(5, 5, deter)
	  val view = new TerrainHexFieldView(field)
	  val additives = TerrainHexViewAdditive.extractAdditives(view.hex(1, 1))
	  assert(additives.size === 2)
	  exist(additives, Directions.NW, Directions.NW, Grass, Sand)
	  exist(additives, Directions.S, Directions.S, Grass, Sand)
  }
  
  test("case with single and multiple terrains") {
      val deter = new TypeDeterminator(Map(((0, 1) -> Sand), ((1, 0) -> Sand), ((2, 1) -> Sand), ((1, 2) -> Sand)))
	  val field = new TerrainHexField(5, 5, deter)
	  val view = new TerrainHexFieldView(field)
	  val additives = TerrainHexViewAdditive.extractAdditives(view.hex(1, 1))
	  assert(additives.size === 2)
	  
	  exist(additives, Directions.NW, Directions.NE, Grass, Sand)
	  exist(additives, Directions.S, Directions.S, Grass, Sand)
  }
  
  test("case on the edge") {
      val deter = new TypeDeterminator(Map(((1, 0) -> Sand), ((2, 1) -> Sand)))
	  val field = new TerrainHexField(5, 5, deter)
	  val view = new TerrainHexFieldView(field)
      val additives = TerrainHexViewAdditive.extractAdditives(view.hex(2, 0))
	  assert(additives.size === 1)
	  exist(additives, Directions.S, Directions.SW, Grass, Sand)
  }
  
  test("normalizing when covering full circle") {
    val add1 = new TerrainHexViewAdditive(Directions.N, Directions.NE, Grass, Sand)
    val add2 = new TerrainHexViewAdditive(Directions.NW, Directions.N, Grass, Sand)
    val add3 = new TerrainHexViewAdditive(Directions.SE, Directions.N, Grass, Sand)
    val add4 = new TerrainHexViewAdditive(Directions.S, Directions.SE, Grass, Sand)
    
    assert(add1.from === Directions.N)
    assert(add1.to === Directions.NE)
    
    assert(add2.from === Directions.NW)
    assert(add2.to === Directions.N)
    
    assert(add3.from === Directions.SE)
    assert(add3.to === Directions.N)
        
    assert(add4.from === Directions.N)
    assert(add4.to === Directions.NW)    
  }
  
  test("case with water") {
    val deter = new TypeDeterminator(Map(((1, 0) -> Sand), ((2, 1) -> Water), ((2, 2) -> Water)))
	val field = new TerrainHexField(3, 5, deter)
	val view = new TerrainHexFieldView(field)
	
    val additives = TerrainHexViewAdditive.extractAdditives(view.hex(1, 1))
	assert(additives.size === 2)
	exist(additives, Directions.N, Directions.N, Grass, Sand)
	exist(additives, Directions.NE, Directions.SE, Grass, BankOutside)
    
	val additivesOnWater = TerrainHexViewAdditive.extractAdditives(view.hex(2, 1))
    assert(additivesOnWater.size === 1)
	exist(additivesOnWater, Directions.SW, Directions.N, Water, BankInside)	
  }
}

class TypeDeterminator(map:Map[(Int, Int), TerrainType], default:TerrainType = Grass) 
	extends Function2[Int, Int, TerrainHex]{
  
  def apply(x:Int, y:Int):TerrainHex = {
    val ter = map.get((x, y))
    val terType = ter match {
      case Some(t) => t
      case None => default
    }
    
    new TerrainHex(x, y, terType)
  }
}