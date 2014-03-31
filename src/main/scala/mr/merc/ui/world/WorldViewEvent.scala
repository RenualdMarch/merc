package mr.merc.ui.world

import mr.merc.map.hex.view.TerrainHexView
import mr.merc.map.world.Province

sealed trait WorldViewEvent {

}

case class ShowCityArrowsWorldViewEvent(arrows: List[(Province, Province)]) extends WorldViewEvent