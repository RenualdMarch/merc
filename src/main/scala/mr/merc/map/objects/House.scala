package mr.merc.map.objects

abstract class House(name:String) extends OneImageMapObject(name)

object HumanCityHouse extends House("humanCity")
object HumanVillageHouse extends House("humanVillage")
object HumanCottage extends House("humanCottage")