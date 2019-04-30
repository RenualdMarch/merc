package mr.merc.unit

import mr.merc.map.terrain._

import scala.xml.XML

object SoldierType {
  val list = parseTypes
  private val map = list.map(st => (st.name, st)).toMap

  def apply(name: String) = map(name)

  private def parseTypes: List[SoldierType] = {
    val xml = XML.load(getClass.getResourceAsStream("/conf/soldierTypes.xml"))
    val parsed = (xml \ "soldierType").map(node => {
      val name = (node \ "@name").toString()
      val cost = (node \ "@cost").toString().toInt
      val hp = (node \ "@hp").toString().toInt
      val movement = (node \ "@movement").toString().toInt
      val exp = (node \ "@exp").toString().toInt
      val level = (node \ "@level").toString().toInt
      val attributes = split((node \ "@attributes").toString()).map(SoldierTypeAttribute.apply)

      var index = -1
      val attacks = (node \ "attacks" \ "attack").map(attackNode => {
        val damage = (attackNode \ "@damage").toString().toInt
        val count = (attackNode \ "@count").toString().toInt
        val ranged = (attackNode \ "@ranged").toString().toBoolean
        val attackTypeName = (attackNode \ "@attackType").toString()
        val attackType = AttackType(attackTypeName)
        val attributes = split((attackNode \ "@attributes").toString()).map(AttackAttribute.apply)
        val projectileStr = (attackNode \ "@projectile").toString()
        val projectile = if (projectileStr.isEmpty()) None else Some(projectileStr)
        index += 1
        new Attack(index, damage, count, attackType, ranged, attributes)
      }).toList

      val moveCostsMap = (node \ "moveCosts" \ "moveCost").map(costNode => {
        val terrainType = parseTerrainKind((costNode \ "@type").toString())
        val cost = (costNode \ "@cost").toString().toInt
        (terrainType, cost)
      }).toMap

      val defencesMap = (node \ "defences" \ "defence").map(defenceNode => {
        val terrainType = DefenceType((defenceNode \ "@type").toString()).getOrElse(
          sys.error(s"Failed to get defence type for [${(defenceNode \ "@type").toString()}]"))
        val defence = (defenceNode \ "@defence").toString().toInt
        (terrainType, defence)
      }).toMap

      val resistancesMap = (node \ "resistances" \ "resistance").map(resNode => {
        val attackType = AttackType((resNode \ "@type").toString())
        val resistance = (resNode \ "@resistance").toString().toInt
        (attackType, resistance)
      }).toMap withDefault (a => 0)

      new SoldierType(name, cost, hp, movement, exp, level, attacks, moveCostsMap, defencesMap, resistancesMap, attributes, name)
    })

    parsed.toList
  }

  private def split(line: String) = if (line.isEmpty) {
    Set()
  } else {
    line.split(",").map(_.trim()) toSet
  }

  def parseTerrainKind(name: String):TerrainKind = {
    val map = Map("Water" -> WaterKind,
    "Forest" -> ForestKind,
    "Swamp" -> SwampKind,
    "Hill" -> HillKind,
    "Mountain" -> MountainKind,
    "Sand" -> SandKind,
    "Grass" -> GrassKind,
    "Building" -> WallsKind)

    map.getOrElse(name, sys.error(s"Failed to parse Terrain kind for $name"))
  }
}

case class SoldierType(name: String, cost: Int, hp: Int, movement: Int, exp: Int, level: Int,
                       attacks: List[Attack], moveCost: Map[TerrainKind, Int], defence: Map[DefenceType, Int],
                       resistance: Map[AttackType, Int], attributes: Set[SoldierTypeAttribute] = Set(), viewName: String) {
}

sealed trait DefenceType

object DefenceType {

  def apply(name: String): Option[DefenceType] = Map[String, DefenceType](
    "Water" -> WaterDefence,
    "Forest" -> ForestDefence,
    "Swamp" -> SwampDefence,
    "Hill" -> HillDefence,
    "Mountain" -> MountainDefence,
    "Sand" -> SandDefence,
    "Grass" -> GrassDefence,
    "Building" -> BuildingDefence).get(name)
}

case object WaterDefence extends DefenceType
case object ForestDefence extends DefenceType
case object SwampDefence extends DefenceType
case object HillDefence extends DefenceType
case object MountainDefence extends DefenceType
case object SandDefence extends DefenceType
case object GrassDefence extends DefenceType
case object BuildingDefence extends DefenceType