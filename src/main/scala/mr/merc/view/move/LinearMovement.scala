package mr.merc.view.move

// speed means pixels per second
class LinearMovement(x1: Int, y1: Int, x2: Int, y2: Int, speed: Int, percentage: Double = 1.0) extends Movement {
  private var timePassed = 0 // in ms

  override def update(time: Int) {
    super.update(time)
    timePassed += time
  }

  private val distance = percentage * scala.math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
  private val timeToCover = distance * 1000 / speed toInt

  def coveredPart: Double = {
    val covered = timePassed.toDouble / timeToCover
    if (covered > 1) 1 else covered
  }

  def destination = (x1 + percentage * (x2 - x1) toInt, y1 + percentage * (y2 - y1) toInt)

  def isOver: Boolean = timePassed >= timeToCover

  def x: Int = x1 + coveredPart * percentage * (x2 - x1) toInt
  def y: Int = y1 + coveredPart * percentage * (y2 - y1) toInt
}