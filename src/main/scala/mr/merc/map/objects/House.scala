package mr.merc.map.objects

import mr.merc.economics.Culture

abstract class House(c:Culture, suffix:String) extends OneImageMapObject(c.houseStyle + suffix) {
  def culture:Culture
}

case class SummerHouse(culture: Culture) extends House(culture, "")

case class WinterHouse(culture: Culture) extends House(culture, "Snow")