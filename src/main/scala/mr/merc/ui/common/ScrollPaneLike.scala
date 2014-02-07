package mr.merc.ui.common

import scalafx.beans.property.DoubleProperty
import scalafx.beans.property.ReadOnlyDoubleProperty

trait ScrollPaneLike {
  val vvalue: DoubleProperty
  val hvalue: DoubleProperty
  def width: ReadOnlyDoubleProperty
  def height: ReadOnlyDoubleProperty
}