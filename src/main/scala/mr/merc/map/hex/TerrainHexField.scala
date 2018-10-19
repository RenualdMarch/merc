package mr.merc.map.hex

import scala.reflect.ClassTag


abstract class AbstractTerrainHexField[T <: TerrainHex: ClassTag](width: Int, height: Int, init: (Int, Int) => T) extends HexField[T](width, height, init)
  with SoldierChangeListener[T]

class TerrainHexField(width: Int, height: Int, init: (Int, Int) => TerrainHex) extends AbstractTerrainHexField[TerrainHex](width, height, init)