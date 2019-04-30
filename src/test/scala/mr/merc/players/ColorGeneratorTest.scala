package mr.merc.players

import org.scalatest.FunSuite

import scalafx.scene.paint.Color

class ColorGeneratorTest extends FunSuite {
  test("first 8 colors") {
    val colorGenerator = new ColorGenerator()
    val list = List.fill(8)(colorGenerator.nextColor())
    assert(list.contains(Color.White))
    assert(list.contains(Color.Black))
    assert(list.contains(Color.Red))
    assert(list.contains(Color.Blue))
  }
}
