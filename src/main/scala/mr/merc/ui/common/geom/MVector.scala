package mr.merc.ui.common.geom

case class MVector(x: Double, y: Double) {
  def *(other: MVector) = this.x * other.x + this.y * other.y
  def *(v: Double) = MVector(x * v, y * v)
  def +(other: MVector) = MVector(this.x + other.x, this.y + other.y)
  def -(other: MVector) = this + other * (-1)
  def norm: MVector = {
    MVector(x / length, y / length)
  }
  def ortho: MVector = {
    if (x == 0) {
      MVector(1, 0)
    } else if (y == 0) {
      MVector(0, 1)
    } else {
      // formula is x1*x2 + y1*y2=0
      // if x2 = 1,x1 and y1 are known, then
      // y2 = -x1/y1
      MVector(1, -x / y).norm
    }
  }

  def toLine(beginX: Double, beginY: Double) = Line(beginX, beginY, beginX + x, beginY + y)

  val length = Math.sqrt(x * x + y * y)
}