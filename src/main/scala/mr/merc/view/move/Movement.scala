package mr.merc.view.move

import mr.merc.unit.view.SoldierView

trait Movement {
    def start()
    def update(time:Int)
	def isOver:Boolean
	// who is first should be rendered first
	def soldiers:List[SoldierView] = Nil
}