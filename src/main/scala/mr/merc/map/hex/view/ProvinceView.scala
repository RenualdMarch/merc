package mr.merc.map.hex.view

import mr.merc.map.hex.{TerrainHex, TerrainHexField}
import mr.merc.map.objects.{House, MapObject}
import mr.merc.map.terrain._
import mr.merc.politics.Province

class ProvinceView(province: Province, field: TerrainHexField, view: TerrainHexFieldView) {

  def refreshCity(): Unit = {
    val houses = province.hexes.filter(_.mapObj.exists(_.isInstanceOf[House]))
    houses.foreach { h =>
      h.mapObj = None
      view.setTerrainDirty(h.x, h.y)
    }

    val newHousesCount = province.regionPopulation.cultureMembers.mapValues { c =>
      math.sqrt(c / 1000).toInt + 1
    }

    val hexesStream = field.closest(province.capital).
      filter(t => !Set[TerrainType](Water, Mountain, Forest, Castle).contains(t.terrain)).
      filter(province.hexes.contains).
      grouped(2).map(_.head).toStream
    val (_, newHouses) = newHousesCount.foldLeft((hexesStream, List[(TerrainHex, MapObject)]())) {
      case ((stream, list), (culture, count)) =>
        val houses = stream.take(count).map(h => (h, culture.houseStyle)).toList ::: list
        (stream.drop(count), houses)
    }

    newHouses.foreach { case (h, m) =>
      h.mapObj = Some(m)
      h.terrain = Road
    }
    newHouses.map(_._1).foreach { h =>
      view.setTerrainDirty(h.x, h.y)
    }
  }
}
