package mr.merc.map.hex

import org.scalatest.FunSuite

class HexFieldTest extends FunSuite {
	val field = new HexField[Hex](3, 4, Hex.hexInit)
	val bigField = new HexField[Hex](6, 5, Hex.hexInit)
  
	test("legal coords") {
	  assert(field.isLegalCoords(0, 0))
	  assert(field.isLegalCoords(1, 0))
	  assert(field.isLegalCoords(2, 0))
	  assert(field.isLegalCoords(0, 1))
	  assert(field.isLegalCoords(1, 1))
	  assert(!field.isLegalCoords(1, 3))
	  assert(field.isLegalCoords(2, 2))
	  assert(!field.isLegalCoords(2, 4))
	  assert(!field.isLegalCoords(1, 4))
	  assert(!field.isLegalCoords(3, 0))
	}
	
	test("hex retrieval") {
	  val hex = field.hex(1, 2)
	  assert(hex.x === 1)
	  assert(hex.y === 2)
	}
	
	test("illegal hex retrieval") {
	  intercept[IllegalArgumentException]{ 
		  field.hex(1, 3)
	  }
	}
	
	test("neighbours in the middle on even row") {
	  val set = bigField.neighbours(1, 1)

	  assert(set.size === 6)
	  assertContainsHex(set, 0, 1)
	  assertContainsHex(set, 1, 0)
	  assertContainsHex(set, 2, 1)
	  assertContainsHex(set, 0, 2)
	  assertContainsHex(set, 1, 2)
	  assertContainsHex(set, 2, 2)
	  
	}
	
	test("neighbours in the middle on odd row") {
	  val set = bigField.neighbours(2, 2)
	  
	  assert(set.size === 6)
	  assertContainsHex(set, 1, 1)
	  assertContainsHex(set, 2, 1)
	  assertContainsHex(set, 3, 1)
	  assertContainsHex(set, 1, 2)
	  assertContainsHex(set, 2, 3)
	  assertContainsHex(set, 3, 2)
	  
	}
	
	test("neighbours on the edges") {
	  val set = field.neighbours(0, 1)
	  assert(set.size === 4)
	  assertContainsHex(set, 0, 0)
	  assertContainsHex(set, 0, 2)
	  assertContainsHex(set, 1, 0)
	  assertContainsHex(set, 1, 1)
	}
	
	def assertContainsHex(set:Traversable[Hex], x:Int, y:Int) {
	  val result = set.exists(h => h.x == x && h.y == y)
	  assert(result, s"$set doesn't contain hex ($x, $y)")
	}
	
	test("cubic and offset conversions") {
	  val hex = bigField.hex(2, 2)
	  val convertedHex = hex.toCubeHex.toHex
	  assert(hex.x === convertedHex.x)
	  assert(hex.y === convertedHex.y)
	}
	
	test("distance between neighbours") {
	  val first = bigField.hex(2, 2)
	  val second = bigField.hex(2, 3)
	  
	  assert(bigField.distance(first, second) === 1)
	  
	  val third = bigField.hex(3, 3)
	  assert(bigField.distance(first, third) === 2)
	}
	
	test("hexes") {
	  val hexes = field.hexes
	  
	  assertContainsHex(hexes, 0, 0)
	  assertContainsHex(hexes, 1, 0)
	  assertContainsHex(hexes, 2, 0)
	  assertContainsHex(hexes, 0, 1)
	  assertContainsHex(hexes, 1, 1)
	  assertContainsHex(hexes, 2, 1)
	  assertContainsHex(hexes, 0, 2)
	  assertContainsHex(hexes, 1, 2)
	  assertContainsHex(hexes, 2, 2)
	  assertContainsHex(hexes, 0, 3)
	  assertContainsHex(hexes, 2, 3)
	  assert(hexes.size === 11)
	}
}