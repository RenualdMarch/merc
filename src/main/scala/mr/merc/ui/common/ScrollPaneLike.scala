package mr.merc.ui.common

import scalafx.beans.property.{DoubleProperty, ObjectProperty, ReadOnlyDoubleProperty, ReadOnlyObjectProperty}

trait ScrollPaneLike {
  val vvalue: DoubleProperty
  val hvalue: DoubleProperty
  def width: ReadOnlyDoubleProperty
  def height: ReadOnlyDoubleProperty
}