package mr.merc.ui.world

import mr.merc.map.world.Province

sealed trait WorldModelEvent {

}

case class MoveToAnotherCityWorldModelEvent(province: Province) extends WorldModelEvent