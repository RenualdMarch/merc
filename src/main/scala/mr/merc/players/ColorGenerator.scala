package mr.merc.players

import mr.merc.log.Logging
import scalafx.scene.paint.Color

import scala.collection.mutable

object ColorGenerator {

  def colorStream:Stream[Color] = {
    val generator = new ColorGenerator()
    Stream.continually (generator.nextColor())
  }
}

class ColorGenerator extends Logging {
  private var stage:Int = -1

  private val notSelectedColors = mutable.Set[Color]()

  private val selectedColors = mutable.Set[Color]()

  private def possibleColorsCount = BigDecimal(2).pow(stage)

  private def mostDistantFromNotSelected: Color = {
    notSelectedColors.maxBy { c =>
      selectedColors.map(s =>
        math.abs(s.red - c.red) + math.abs(s.green - c.green) + math.abs(s.blue - c.blue)
      ).sum
    }
  }

  def nextColor(): Color = {
    if (notSelectedColors.isEmpty) {
      stage += 1
      val interval = BigDecimal(0xFF) / possibleColorsCount
      val newAndOld = for {
        r <- BigDecimal(0).to(0xFF).by(interval)
        g <- BigDecimal(0).to(0xFF).by(interval)
        b <- BigDecimal(0).to(0xFF).by(interval)
      } yield Color.rgb(r.rounded.toInt, g.rounded.toInt, b.rounded.toInt)
      notSelectedColors ++= newAndOld.toSet -- selectedColors
      nextColor()
    } else {
      val max = mostDistantFromNotSelected
      info(s"generated new color $max")
      notSelectedColors -= max
      selectedColors += max
      max
    }

  }
}