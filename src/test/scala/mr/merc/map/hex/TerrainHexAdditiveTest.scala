package mr.merc.map.hex

import org.scalatest.FunSuite
import mr.merc.map.terrain.TerrainType
import mr.merc.map.terrain.Grass
import mr.merc.map.terrain.Hill
import mr.merc.map.terrain.Sand

class TerrainHexAdditiveTest extends FunSuite {
  
  def exist(tr:Traversable[TerrainHexViewAdditive], from:Directions.Direction, to:Directions.Direction, hexType:TerrainType, tp:TerrainType) {
    assert(tr.exists(ad => ad.from == from && ad.to == to && ad.neighbourTerrainType == tp && hexType == ad.hexTerrainType))
  }
  
  test("case with different single terrains") {
	  val deter = new TypeDeterminator(Map(((0, 1) -> Hill), ((2, 1) -> Sand), ((1, 2) -> Hill)))
	  val field = new TerrainHexField(5, 5, deter)
	  val view = new TerrainHexFieldView(field)
	  val additives = TerrainHexViewAdditive.extractAdditives(view.hex(1, 1))
	  assert(additives.size === 3)
	  exist(additives, Directions.NW, Directions.NW, Grass, Hill)
	  exist(additives, Directions.NE, Directions.NE, Grass, Sand)
	  exist(additives, Directions.S, Directions.S, Grass, Hill)
  }
  
  test("case with single and multiple terrains") {
      val deter = new TypeDeterminator(Map(((0, 1) -> Hill), ((1, 0) -> Hill), ((2, 1) -> Hill), ((2, 2) -> Sand)))
	  val field = new TerrainHexField(5, 5, deter)
	  val view = new TerrainHexFieldView(field)
	  val additives = TerrainHexViewAdditive.extractAdditives(view.hex(1, 1))
	  assert(additives.size === 2)
	  exist(additives, Directions.NE, Directions.NW, Grass, Hill)
	  exist(additives, Directions.SE, Directions.SE, Grass, Sand)
  }
  
  test("case on the edge") {
     val deter = new TypeDeterminator(Map(((1, 0) -> Hill), ((2, 1) -> Hill)))
	  val field = new TerrainHexField(5, 5, deter)
	  val view = new TerrainHexFieldView(field)
      val additives = TerrainHexViewAdditive.extractAdditives(view.hex(2, 0))
	  assert(additives.size === 1)
	  exist(additives, Directions.S, Directions.SW, Grass, Hill)
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