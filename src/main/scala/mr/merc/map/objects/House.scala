package mr.merc.map.objects

import mr.merc.economics.Culture

case class House(val culture:Culture) extends OneImageMapObject(culture.houseStyle)