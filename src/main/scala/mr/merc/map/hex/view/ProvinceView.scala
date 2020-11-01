package mr.merc.map.hex.view

import mr.merc.economics.{Culture, EconomicRegion, FourSeasonsTerrainHex, FourSeasonsTerrainHexField}
import mr.merc.politics.Province
import mr.merc.economics.MapUtil.NumericOperations._
import mr.merc.economics.Seasons.Season
import mr.merc.unit.Soldier
import mr.merc.unit.view.SoldierView
import mr.merc.economics.WorldConstants.Population._
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.terrain.FourSeasonsMapObjects._
import mr.merc.map.terrain.FourSeasonsTerrainTypes._

class ProvinceView(season: Season, val province: Province, field: FourSeasonsTerrainHexField, currentField:TerrainHexField, view: TerrainHexFieldView) {

  private def housesPerCulture(map:Map[Culture, Int]):Map[FourSeasonsHouse, Int] = {
    val (minorityWithoutHouses, majorityWithHouses) = map.partition(_._2 < HousePerPopulation)

    val majMap = majorityWithHouses.map { case (cul, count) =>
      FourSeasonsHouse(cul) -> count / HousePerPopulation
    }

    val minMap:Map[FourSeasonsHouse, Int] = if (minorityWithoutHouses.isEmpty) Map()
    else {
      Map(FourSeasonsHouse(minorityWithoutHouses.maxBy(_._2)._1) -> 1)
    }

    majMap |+| minMap
  }

  def refreshCity(): Unit = {
    val alreadyHouses = province.hexes.filter(_.mapObj.exists(_.isInstanceOf[FourSeasonsHouse]))

    val alreadyHousesGroups = alreadyHouses.groupBy(_.mapObj.get).mapValues(_.size).asInstanceOf[Map[FourSeasonsHouse, Int]]

    val newHousesCount = housesPerCulture(province.regionPopulation.cultureMembers)

    val changes = newHousesCount |-| alreadyHousesGroups

    val (plus, minus) = changes.partition(_._2 > 0)

    val hexesStream = field.closest(province.capital).
      filterNot(h => h.terrainMap == FourSeasonsWater || h.terrainMap == FourSeasonsRiver ||
      h.terrainMap == FourSeasonsMountain || h.terrainMap == FourSeasonsDecForest || h.terrainMap == FourSeasonsCastle).
      filter(province.hexes.contains)

    val housesStream = hexesStream.grouped(2).map(_.head).toStream

    val objectsStream = hexesStream.grouped(2).map(_.last).toStream

    objectsStream.take(1).head.mapObj = Some(FourSeasonsSignpost)

    minus.foreach { case (house, count) =>
      housesStream.filter(_.mapObj.contains(house)).take(-count).foreach { h =>
        h.mapObj = None
        currentField.hex(h.x, h.y).mapObj = None
        view.setTerrainDirty(view.hex(h))
      }
    }

    plus.foreach { case (house, count) =>
      housesStream.filter(_.mapObj.isEmpty).take(count).foreach { h =>
        h.mapObj = Some(house)
        h.terrainMap = FourSeasonsRoad
        currentField.hex(h.x, h.y).mapObj = h.mapObj.flatMap(_.mapObject(season))
        currentField.hex(h.x, h.y).terrain = FourSeasonsRoad(season)
        view.setTerrainDirty(view.hex(h))
      }
    }
  }

  def neighbourHexes(neigbour:EconomicRegion):Set[FourSeasonsTerrainHex] = {
    province.hexes.filter { h =>
      field.neighboursSet(h).exists(_.province.contains(neigbour))
    }
  }

  def hexesForSoldiers(destination: Option[EconomicRegion]): Stream[FourSeasonsTerrainHex] = {
    def filter(stream: Stream[FourSeasonsTerrainHex]): Stream[FourSeasonsTerrainHex] = stream.
      filterNot(h => h.terrainMap == FourSeasonsMountain || h.terrainMap == FourSeasonsDecForest ||
        h.terrainMap == FourSeasonsWater || h.terrainMap == FourSeasonsRiver).
      filter(_.mapObj.isEmpty).
      filter(_.soldier.isEmpty)

    destination match {
      case None => filter(field.closest(province.capital))
      case Some(p) => filter(field.closest(neighbourHexes(p)))
    }
  }

  private var currentMap:Map[Soldier, SoldierView] = Map()

  def cleanSoldiers(map:Map[Soldier, SoldierView]): Unit = {
    val map = view.soldiersDrawer.soldiers.map(s => s.soldier -> s).toMap
    province.hexes.foreach { h =>
      h.soldier.flatMap(map.get).foreach { soldier =>
        view.soldiersDrawer.removeSoldier(soldier)
      }
      h.soldier = None
      currentField.hex(h.x, h.y).soldier = None
    }
    currentMap = Map()
  }

  def refreshSoldiers(): Unit = {
    cleanSoldiers(currentMap)
    currentMap = province.regionWarriors.allWarriors.map(w => w.soldier -> w.soldierView(1d, true, false)).toMap
    province.regionWarriors.warriorDestinations.foreach { case (destination, soldiers) =>
      (hexesForSoldiers(destination) zip soldiers).foreach { case (h, w) =>
          h.soldier = Some(w.soldier)
          currentField.hex(h.x, h.y).soldier = Some(w.soldier)
          val sv = currentMap(w.soldier)
          sv.coords = view.hex(h).coords
          view.soldiersDrawer.addSoldier(sv)
      }
    }
  }
}
