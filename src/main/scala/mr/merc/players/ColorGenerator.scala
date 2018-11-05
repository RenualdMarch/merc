package mr.merc.players

import scala.util.Random
import scalafx.scene.paint.Color

class ColorGenerator {

  private var returnedColors: Set[Color] = Set()

  private var stage = 1d

  private def currentStep = 1 / stage

  private def isOne(a: Double) = math.abs(1 - a) <= 10e-10

  private var r = 0d
  private var g = 0d
  private var b = 0d

  def nextColor(): Color = {
    val result = Color.color(r, g, b)
    if (isOne(r)) {
      if (isOne(g)) {
        if (isOne(b)) {
          stage += 1
          r = 0
          g = 0
          b = 0
        } else {
          b += currentStep
          r = 0
          g = 0
        }
      } else {
        g += currentStep
        r = 0
      }
    } else {
      r += currentStep
    }
    if (returnedColors.contains(result)) {
      nextColor()
    } else {
      returnedColors += result
      result
    }
  }
}