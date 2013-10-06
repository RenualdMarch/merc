package mr.merc.view.move

trait Movement {
    def start()
    def update(time:Int)
	def isOver:Boolean
}