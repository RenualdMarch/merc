package mr.merc.ui.minimap

import org.scalatest.FunSuite

class MinimapSizeTest extends FunSuite {

  test("square field and square minimap") {
    val result = MinimapSize(4, 4, 100, 100)
    assert(result === MinimapSize(25, 100, 100))
  }

  test("square field and rectangular minimap w>h") {
    val result = MinimapSize(4, 4, 200, 100)
    assert(result === MinimapSize(25, 100, 100))
  }

  test("square field and rectangular minimap w<h") {
    val result = MinimapSize(4, 4, 100, 200)
    assert(result === MinimapSize(25, 100, 100))
  }

  test("rectangular w>h field and square minimap") {
    val result = MinimapSize(4, 2, 100, 100)
    assert(result === MinimapSize(25, 100, 50))
  }

  test("rectangular h>w field and square minimap") {
    val result = MinimapSize(2, 4, 100, 100)
    assert(result === MinimapSize(25, 50, 100))
  }

  test("rectangular h>w field and rectangular h>w minimap") {
    val result = MinimapSize(2, 4, 100, 300)
    assert(result === MinimapSize(50, 100, 200))
  }

  test("rectangular h>w field and rectangular w>h minimap") {
    val result = MinimapSize(2, 4, 200, 100)
    assert(result === MinimapSize(25, 50, 100))
  }

  test("rectangular w>h field and rectangular w>h minimap") {
    val result = MinimapSize(4, 2, 300, 100)
    assert(result === MinimapSize(50, 200, 100))

  }
}