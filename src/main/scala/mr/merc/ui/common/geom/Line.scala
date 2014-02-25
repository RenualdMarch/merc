package mr.merc.ui.common.geom

case class Line(beginX: Double, beginY: Double, endX: Double, endY: Double) {
  def cutFromTheBeginning(length: Double): Line = {
    val newLength = toMVector.length - length
    val newVector = toMVector.norm * newLength
    val beginPoints = MVector(endX, endY) - newVector
    Line(beginPoints.x, beginPoints.y, endX, endY)
  }

  def cutFromTheEnd(length: Double): Line = {
    val newLength = toMVector.length - length
    val newVector = toMVector.norm * newLength
    val endPoints = newVector + MVector(beginX, beginY)
    Line(beginX, beginY, endPoints.x, endPoints.y)
  }

  def toMVector = MVector(endX - beginX, endY - beginY)
}