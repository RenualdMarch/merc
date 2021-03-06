package mr.merc.map.objects

import mr.merc.map.objects.view.{MapObjectView, OneImageObjectView}

abstract class MapObject(val name:String) {
	def view:MapObjectView
}

abstract class OneImageMapObject(name: String) extends MapObject(name) {
  @transient override lazy val view: MapObjectView = new OneImageObjectView(name, this)
}

