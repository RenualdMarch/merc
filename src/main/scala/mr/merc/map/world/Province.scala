package mr.merc.map.world

import mr.merc.map.hex.TerrainHex
import mr.merc.map.objects.House

case class Province(settlement: Settlement, hexes: Set[TerrainHex]) {
  def containsHex = hexes.contains _
  val settlementHex = hexes.find(_.mapObj == Some(House)) get
}