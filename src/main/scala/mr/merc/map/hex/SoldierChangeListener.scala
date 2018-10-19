package mr.merc.map.hex

trait SoldierChangeListener[T <: TerrainHex] {
  self:HexField[T] =>

  private var _soldierChangeListener: () => Unit = () => {}
  def soldierChangeListener: () => Unit = _soldierChangeListener
  def soldierChangeListener_=(f: (Int, Int) => Unit) {
    hexes.foreach(_.soldierChangeListener = f)
  }
}
