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

    assert(!list.contains(Color.color(0.5, 0.5, 0.5)))
  }

  test("second 21") {
    val colorGenerator = new ColorGenerator()
    0 until 8 foreach (_ => colorGenerator.nextColor())
    val list = List.fill(19)(colorGenerator.nextColor())
    assert(!list.contains(Color.White, Color.Black, Color.Red, Color.Blue))
    val variants = List(0, 0.5, 1)
    for {
      r <- variants
      g <- variants
      b <- variants
    } {
      if (r == 0.5 || g == 0.5 || b == 0.5)
        assert(list.contains(Color.color(r, g, b)))
    }

  }

}
