package mr.merc.view.move

import mr.merc.view.Drawable

trait Movement {
    def start()
    def update(time:Int)
	def isOver:Boolean
	// who is first should be rendered first
	def drawables:List[Drawable] = Nil
}