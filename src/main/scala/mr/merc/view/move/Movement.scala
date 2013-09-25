package mr.merc.view.move

trait Movement {
	def update(time:Int)
	def isOver:Boolean
}