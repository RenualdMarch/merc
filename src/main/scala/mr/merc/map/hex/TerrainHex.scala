package mr.merc.map.hex

import mr.merc.map.terrain.TerrainType
import mr.merc.map.terrain.Grass
import mr.merc.map.objects.MapObject
import mr.merc.politics.Province
import mr.merc.unit.Soldier

object TerrainHex {
  def grassInit(x: Int, y: Int) = new TerrainHex(x, y, Grass)
}

// TODO extract soldier to TerrainHexWithSoldier
class TerrainHex(x: Int, y: Int, val terrain: TerrainType, val mapObj: Option[MapObject] = None,
                 var province: Option[Province] = None) extends Hex(x, y) {
  private var _soldier: Option[Soldier] = None
  var soldierChangeListener: (Int, Int) => Unit = (x, y) => {}
  def soldier = _soldier
  def soldier_=(s: Option[Soldier]) {
    _soldier = s
    soldierChangeListener(x, y)
  }
}