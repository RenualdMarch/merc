package mr.merc.map.hex.view

import mr.merc.map.hex.TerrainHexField
import mr.merc.map.objects.{House, Signpost}
import mr.merc.map.terrain._
import mr.merc.politics.Province
import mr.merc.economics.MapUtil.MapWithOperations

object ProvinceView {
  val housePerPopulation = 20000
}

class ProvinceView(province: Province, field: TerrainHexField, view: TerrainHexFieldView) {

  def refreshCity(): Unit = {
    val alreadyHouses = province.hexes.filter(_.mapObj.exists(_.isInstanceOf[House]))

    val alreadyHousesGroups = alreadyHouses.groupBy(_.mapObj.get).mapValues(_.size).asInstanceOf[Map[House, Int]]

    val newHousesCount = province.regionPopulation.cultureMembers.map { case (cul, count) =>
      cul.houseStyle -> (count / ProvinceView.housePerPopulation + 1)
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
}
