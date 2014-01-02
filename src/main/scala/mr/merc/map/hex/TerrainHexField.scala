package mr.merc.map.hex

class TerrainHexField(width: Int, height: Int, init: (Int, Int) => TerrainHex) extends HexField[TerrainHex](width, height, init) {
  private var _soldierChangeListener: () => Unit = () => {}
  def soldierChangeListener = _soldierChangeListener
  def soldierChangeListener_=(f: (Int, Int) => Unit) {
    hexes.foreach(_.soldierChangeListener = f)
  }
}