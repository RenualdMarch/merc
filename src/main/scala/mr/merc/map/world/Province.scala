package mr.merc.map.world

import mr.merc.map.hex.TerrainHex
import mr.merc.map.objects.House
import mr.merc.world.character.CharactersInProvince

class Province(val settlement: Settlement, val hexes: Set[TerrainHex]) {
  def containsHex = hexes.contains _
  val settlementHex = hexes.find(_.mapObj == Some(House)) get
  val characters = new CharactersInProvince()

}