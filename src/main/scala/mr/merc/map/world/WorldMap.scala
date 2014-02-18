package mr.merc.map.world

import mr.merc.map.hex.TerrainHexField
import mr.merc.map.objects.House
import mr.merc.world.Country
import scala.xml.XML
import scala.xml.Elem
import mr.merc.map.reader.WesnothMapReader
import mr.merc.world.Culture

object WorldMap {
  def load(mapName: String): WorldMap = {
    val hexField = new WesnothMapReader().readMap(getClass.getResourceAsStream("/maps/" + mapName + ".map"))
    load(hexField, mapName)
  }

  def load(hexField: TerrainHexField, mapName: String): WorldMap = {
    val xml = XML.load(getClass.getResourceAsStream("/maps/" + mapName + ".xml"))
    val settlements = loadSettlements(xml)

    val provinces = calculateProvinces(hexField, settlements)
    val provincesMap = provinces map (p => (p.settlement.nameKey, p)) toMap
    val countries = loadCountries(xml, provincesMap)
    new WorldMap(hexField, provinces, countries)
  }

  private[world] def loadSettlements(xml: Elem): Map[(Int, Int), Settlement] = {
    val settlements = for (node <- xml \ "settlement") yield {
      val x = (node \ "@x").toString().toInt
      val y = (node \ "@y").toString().toInt
      val nameKey = (node \ "@nameKey").toString()
      val cultureName = (node \ "@cultureName").toString()
      val population = (node \ "@population").toString().toInt

      ((x, y), Settlement(nameKey, cultureName, population))
    }
    settlements toMap
  }

  private[world] def calculateProvinces(hexField: TerrainHexField, settlements: Map[(Int, Int), Settlement]): Set[Province] = {
    val centers = hexField.hexes.filter(_.mapObj == Some(House))
    val result = hexField.hexes.groupBy { h =>
      val centersAndDistances = centers.map(c => (c, h.distance(c), settlements(c.x, c.y).population))
      centersAndDistances.minBy(c => (c._2, c._3))._1
    }

    result map (r => new Province(settlements(r._1.x, r._1.y), r._2.toSet)) toSet
  }

  def loadCountries(xml: Elem, provinces: Map[String, Province]): List[Country] = {
    val countries = for (node <- xml \ "country") yield {
      val nameKey = (node \ "@nameKey").toString()
      val cultureName = (node \ "@cultureName").toString()
      val settlements = (node \ "@settlements").toString().split(",").toSet
      val provincesSet = settlements map provinces
      val c = new Country(nameKey, Culture(cultureName))
      c.provinces = provincesSet
      c
    }

    countries toList

  }
}

class WorldMap(val hexField: TerrainHexField, val provinces: Set[Province], val countries: List[Country]) {

  val provinceByHex = provinces.flatMap(p => p.hexes.map(h => (h, p))) toMap

}