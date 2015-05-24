package mr.merc.person

class LevelUpMechanic(initialPoints: Int, formula: LevelUpFormula) {
  private var _level = formula.pointsToLevel(initialPoints)
  private var currentPoints = initialPoints.toDouble
  def level = _level
  def points = currentPoints.toInt
  def addPoints(points: Double) {
    currentPoints += points
    _level = formula.pointsToLevel(currentPoints.toInt)
  }

  def addPoints(points: Int): Unit = addPoints(points.toDouble)
}

class LevelUpFormula(levelToPoints:Int => Int) {

  def pointsToLevel(points: Int): Int = {
    def predicate(level: Int):SearchResult = if (levelToPoints(level + 1) > points &&
      levelToPoints(level) <= points) Found
    else if (levelToPoints(level - 1) >= points) TooBig
    else if (levelToPoints(level + 1) <= points) TooSmall
    else sys.error(s"Impossible case with $level and $points")

    var expectedLevel = 1
    val multiplier = 2
    while(levelToPoints(expectedLevel) < points) {
      expectedLevel *= multiplier
    }

    val lowerBound = expectedLevel / multiplier
    val upperBound = expectedLevel


    def binarySearch(from: Int, to: Int): Int = {
      if (from == to) {
        if (predicate(from) == Found) from
        else sys.error(s"Impossible case: $from is not correct level for $points")
      } else {
        val center = (from + to) / 2
        predicate(center) match {
          case Found =>
            center
          case TooBig =>
            binarySearch(from, center)
          case TooSmall =>
            binarySearch(center + 1, to)
        }
      }
    }

    binarySearch(lowerBound, upperBound)
  }

  def remainToNextLevel(points: Int) = {
    val level = pointsToLevel(points)
    levelToPoints(level + 1) - points
  }

  def pointsAfterThisLevel(points: Int) = {
    val level = pointsToLevel(points)
    points - levelToPoints(level)
  }

  private sealed trait SearchResult
  private object Found extends SearchResult
  private object TooBig extends SearchResult
  private object TooSmall extends SearchResult
}