package mr.merc.ui.minimap

import scala.math._

object MinimapSize {
  def apply(fieldWidth: Int, fieldHeight: Int, minimapWidth: Int, minimapHeight: Int): MinimapSize = {
    val side = min(minimapWidth / fieldWidth.toDouble, minimapHeight / fieldHeight.toDouble)
    MinimapSize(side, (side * fieldWidth).ceil.toInt, (side * fieldHeight).ceil.toInt)
  }
}

case class MinimapSize(cellSide: Double, minimapUsefulHeight: Int, minimapUsefulWidth: Int)