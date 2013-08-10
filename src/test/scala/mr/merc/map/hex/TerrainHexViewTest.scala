package mr.merc.map.hex

import org.scalatest.FunSuite

class TerrainHexViewTest extends FunSuite {
	val field = new TerrainHexField(5, 5, TerrainHex.grassInit)
  
    test("pixel coords") {
	  val view1 = new TerrainHexView(field.hex(0, 0), field)
	  assert(view1.x === 0)
	  assert(view1.y === 0)
	  
	  val view2 = new TerrainHexView(field.hex(1, 0), field)
	  assert(view2.x === 72 * 3 / 4)
	  assert(view2.y === 72 / 2)
	  
	  
	  val view3 = new TerrainHexView(field.hex(1, 1), field)
	  assert(view3.x === 72 * 3 / 4)
	  assert(view3.y === 72 * 3 / 2)
	  
	  val view4 = new TerrainHexView(field.hex(2, 2), field)
	  assert(view4.x === 72 + 72 * 1 / 2)
	  assert(view4.y === 72 * 2)
	  
	  val view5 = new TerrainHexView(field.hex(3, 2), field)
	  assert(view5.x === 2 * 72  + 72 / 4)
	  assert(view5.y === 72 * 2 + 72 / 2)	
	}
}