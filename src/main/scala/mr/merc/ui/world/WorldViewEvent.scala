package mr.merc.ui.world

import mr.merc.map.hex.view.TerrainHexView

sealed trait WorldViewEvent {

}

case class ShowCityArrowsWorldViewEvent(from: TerrainHexView, to: TerrainHexView) extends WorldViewEvent