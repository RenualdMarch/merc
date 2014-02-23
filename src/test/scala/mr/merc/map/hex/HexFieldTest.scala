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
    intercept[IllegalArgumentException] {
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

  def assertContainsHex(set: Traversable[Hex], x: Int, y: Int) {
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

  test("even directions") {
    val even = bigField.hex(1, 1)
    val map = bigField.neighboursWithDirections(even)
    assert(map(N) === Hex(1, 0))
    assert(map(S) === Hex(1, 2))
    assert(map(NE) === Hex(2, 1))
    assert(map(NW) === Hex(0, 1))
    assert(map(SE) === Hex(2, 2))
    assert(map(SW) === Hex(0, 2))
  }

  test("odd directions") {
    val odd = bigField.hex(2, 2)
    val map = bigField.neighboursWithDirections(odd)
    assert(map(N) === Hex(2, 1))
    assert(map(S) === Hex(2, 3))
    assert(map(NE) === Hex(3, 1))
    assert(map(NW) === Hex(1, 1))
    assert(map(SE) === Hex(3, 2))
    assert(map(SW) === Hex(1, 2))
  }

  test("directions on the border") {
    val hex = bigField.hex(2, 0)
    val map = bigField.neighboursWithDirections(hex)
    assert(map.get(N) === None)
    assert(map(S) === Hex(2, 1))
    assert(map.get(NE) === None)
    assert(map.get(NW) === None)
    assert(map(SE) === Hex(3, 0))
    assert(map(SW) === Hex(1, 0))
  }

  test("distance test") {
    val start = bigField.hex(0, 0)
    val finish1 = bigField.hex(0, 3)
    assert(start.distance(finish1) === 3)
    val finish2 = bigField.hex(2, 2)
    assert(start.distance(finish2) === 3)
  }
}