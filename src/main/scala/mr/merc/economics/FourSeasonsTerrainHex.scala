package mr.merc.economics

import mr.merc.map.hex.{Hex, HexField, TerrainHex, TerrainHexField}
import mr.merc.map.objects.MapObject
import mr.merc.map.terrain.TerrainType
import mr.merc.politics.Province
import mr.merc.unit.Soldier

class FourSeasonsTerrainHex(x: Int, y: Int, var terrainMap:Map[Seasons.Season, TerrainType],
                            var mapObj: Option[FourSeasonsMapObject] = None,
                            var province: Option[Province] = None) extends Hex(x, y){

  private var presentSoldier:Option[Soldier] = None

  def soldier:Option[Soldier] = presentSoldier

  def soldier_=(soldierOpt:Option[Soldier]): Unit = {
    presentSoldier = soldierOpt
  }

  def terrainHex(season:Seasons.Season):TerrainHex = {
    val hex = new TerrainHex(x, y, terrainMap(season), mapObj.flatMap(_.mapObject(season)), province)
    hex.soldier = presentSoldier
    hex
  }
}

class FourSeasonsMapObject(objects:Map[Seasons.Season, MapObject]) {
  def mapObject(season:Seasons.Season):Option[MapObject] = objects.get(season)
}

class FourSeasonsTerrainHexField(width:Int, height:Int, f:(Int, Int) => FourSeasonsTerrainHex)
  extends HexField[FourSeasonsTerrainHex](width, height, f) {

  def buildTerrainHexField(season:Seasons.Season):TerrainHexField = {
    new TerrainHexField(width, height, (x, y) => hex(x, y).terrainHex(season))
  }
}