package mr.merc.map

trait Grid[T] {
	def isBlocked(t:T) = false
	def distance(from:T, to:T):Int
	def neighbours(t:T):Set[T]
	def price(t:T) = 1
}