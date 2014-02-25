package mr.merc.ui.common.geom

case class MVector(x: Double, y: Double) {
  def *(other: MVector) = this.x * other.x + this.y * other.y
  def *(v: Double) = MVector(x * v, y * v)
  def +(other: MVector) = MVector(this.x + other.x, this.y + other.y)
  def -(other: MVector) = this + other * (-1)
  def norm: MVector = {
    MVector(x / length, y / length)
  }

  val length = Math.sqrt(x * x + y * y)
}