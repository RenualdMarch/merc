package mr.merc.map.world

import mr.merc.map.hex.TerrainHexField
import mr.merc.map.objects.House

class WorldMap(hexField: TerrainHexField, mapName: String) {

  // TODO divide into provinces using population info
  lazy val provinces: Set[Province] = {
    val settlements = Settlement.loadSettlements(mapName)
    val centers = hexField.hexes.filter(_.mapObj == Some(House))
    val result = hexField.hexes.groupBy { h =>
      val centersAndDistances = centers.map(c => (c, h.distance(c), settlements(c.x, c.y).population))
      centersAndDistances.minBy(c => (c._2, c._3))._1
    }

    result map (r => new Province(settlements(r._1.x, r._1.y), r._2.toSet)) toSet
  }

  lazy val provinceByHex = provinces.flatMap(p => p.hexes.map(h => (h, p))) toMap
}