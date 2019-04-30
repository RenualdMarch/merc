package mr.merc.map.hex.view

import mr.merc.economics.EconomicRegion
import mr.merc.map.hex.{TerrainHex, TerrainHexField}
import mr.merc.map.objects.{House, Signpost, WoodenBridge}
import mr.merc.map.terrain._
import mr.merc.politics.Province
import mr.merc.economics.MapUtil.NumericOperations._
import mr.merc.unit.Soldier
import mr.merc.unit.view.SoldierView
import mr.merc.economics.WorldConstants.Population._

class ProvinceView(province: Province, field: TerrainHexField, view: TerrainHexFieldView) {

  def refreshCity(): Unit = {
    val alreadyHouses = province.hexes.filter(_.mapObj.exists(_.isInstanceOf[House]))

    val alreadyHousesGroups = alreadyHouses.groupBy(_.mapObj.get).mapValues(_.size).asInstanceOf[Map[House, Int]]

    val newHousesCount = province.regionPopulation.cultureMembers.map { case (cul, count) =>
      cul.houseStyle -> (count / HousePerPopulation + 1)
    }

    val changes = newHousesCount |-| alreadyHousesGroups

    val (plus, minus) = changes.partition(_._2 > 0)

    val hexesStream = field.closest(province.capital).
      filterNot(_.terrain.isOneOf(WaterKind, MountainKind, ForestKind, WallsKind)).
      filter(province.hexes.contains)

    val housesStream = hexesStream.grouped(2).map(_.head).toStream

    val objectsStream = hexesStream.grouped(2).map(_.last).toStream

    objectsStream.take(1).head.mapObj = Some(Signpost)

    minus.foreach { case (house, count) =>
      housesStream.filter(_.mapObj.contains(house)).take(-count).foreach { h =>
        h.mapObj = None
        view.setTerrainDirty(view.hex(h))
      }
    }

    plus.foreach { case (house, count) =>
      housesStream.filter(_.mapObj.isEmpty).take(count).foreach { h =>
        h.mapObj = Some(house)
        h.terrain = GrassyRoad
        view.setTerrainDirty(view.hex(h))
      }
    }
  }

  def neighbourHexes(neigbour:EconomicRegion):Set[TerrainHex] = {
    province.hexes.filter { h =>
      field.neighboursSet(h).exists(_.province.contains(neigbour))
    }
  }

  def hexesForSoldiers(destination: Option[EconomicRegion]): Stream[TerrainHex] = {
    def filter(stream: Stream[TerrainHex]): Stream[TerrainHex] = stream.
      filterNot(_.terrain.isOneOf(MountainKind, ForestKind, WaterKind)).
      filter(_.mapObj.isEmpty).
      filter(_.soldier.isEmpty)

    destination match {
      case None => filter(field.closest(province.capital))
      case Some(p) => filter(field.closest(neighbourHexes(p)))
    }
  }

  private var currentMap:Map[Soldier, SoldierView] = Map()
  def cleanSoldiers(map:Map[Soldier, SoldierView]): Unit = {
    province.hexes.foreach { h =>
      h.soldier.foreach {s =>
        view.soldiersDrawer.removeSoldier(map(s))
      }
      h.soldier = None
    }
  }

  def refreshSoldiers(): Unit = {
    cleanSoldiers(currentMap)
    currentMap = province.regionWarriors.allWarriors.map(w => w.soldier -> w.soldierView(1d, false)).toMap
    province.regionWarriors.warriorDestinations.foreach { case (destination, soldiers) =>
      (hexesForSoldiers(destination) zip soldiers).foreach { case (h, w) =>
          h.soldier = Some(w.soldier)
          val sv = currentMap(w.soldier)
          sv.coords = view.hex(h).coords
          view.soldiersDrawer.addSoldier(sv)
      }
    }
  }
}
