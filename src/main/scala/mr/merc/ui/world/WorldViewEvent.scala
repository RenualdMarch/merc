package mr.merc.ui.world

import mr.merc.map.hex.view.TerrainHexView
import mr.merc.map.world.Province

sealed trait WorldViewEvent {

}

// TODO replace province with TerrainHexView
case class ShowCityArrowsWorldViewEvent(arrows: List[(Province, Province)]) extends WorldViewEvent
case class SelectCityArrow(from: TerrainHexView, to: TerrainHexView) extends WorldViewEvent
case object DeselectCityArrow extends WorldViewEvent