package mr.merc.map

trait Grid[T] {
  def isBlocked(t: T):Boolean
  def price(from: T, to: T): Double
  def neighbours(t: T): Set[T]
}

trait PossibleGrid[T] extends Grid[T] {
  def cellWhereMovementMustBeStopped(t: T): Boolean
  def cellWhereItIsForbiddenToStop(t: T): Boolean
}

trait ShortestGrid[T] extends Grid[T] {
  def heuristic(from: T, to: T):Double
}

trait UniversalGrid[T] extends PossibleGrid[T] with ShortestGrid[T]