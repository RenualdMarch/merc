package mr.merc.map

trait Grid[T] {
  def isBlocked(t: T) = false
  def neighbours(t: T): Set[T]
  def price(from: T, to: T) = 1
  def cellWhereMovementMustBeStopped(t: T) = false
  def cellWhereItIsForbiddenToStop(t: T) = false
}