package mr.merc.map.world

import mr.merc.map.hex.TerrainHex

case class Province(settlement: Settlement, hexes: Set[TerrainHex]) {
  def containsHex = hexes.contains _
}