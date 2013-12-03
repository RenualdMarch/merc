package mr.merc.unit

import mr.merc.map.terrain.TerrainType
import scala.xml.XML
import java.io.File
import mr.merc.unit.view.Projectile

object SoldierType {
  val list = parseTypes
  private val map = list.map(st => (st.name, st)).toMap

  def apply(name: String) = map(name)

  private def parseTypes: List[SoldierType] = {
    val path = getClass.getResource("/conf/soldierTypes.xml").toURI
    val xml = XML.loadFile(new File(path))
    val parsed = (xml \ "soldierType").map(node => {
      val name = (node \ "@name").toString()
      val cost = (node \ "@cost").toString().toInt
      val hp = (node \ "@hp").toString().toInt
      val movement = (node \ "@movement").toString().toInt
      val exp = (node \ "@exp").toString().toInt
      val level = (node \ "@level").toString().toInt
      val attributes = split((node \ "@attributes").toString()).map(SoldierTypeAttribute.apply)

      val attacks = (node \ "attacks" \ "attack").map(attackNode => {
        val imageName = (attackNode \ "@imageName").toString()
        val damage = (attackNode \ "@damage").toString().toInt
        val count = (attackNode \ "@count").toString().toInt
        val ranged = (attackNode \ "@ranged").toString().toBoolean
        val attackTypeName = (attackNode \ "@attackType").toString()
        val attackType = AttackType(attackTypeName)
        val attributes = split((attackNode \ "@attributes").toString()).map(AttackAttribute.apply)
        val projectileStr = (attackNode \ "@projectile").toString()
        val projectile = if (projectileStr.isEmpty()) None else Some(projectileStr)
        new Attack(imageName, damage, count, attackType, ranged, attributes, projectile)
      }).toList

      val moveCostsMap = (node \ "moveCosts" \ "moveCost").map(costNode => {
        val terrainType = TerrainType((costNode \ "@type").toString())
        val cost = (costNode \ "@cost").toString().toInt
        (terrainType, cost)
      }).toMap

      val defencesMap = (node \ "defences" \ "defence").map(defenceNode => {
        val terrainType = TerrainType((defenceNode \ "@type").toString())
        val defence = (defenceNode \ "@defence").toString().toInt
        (terrainType, defence)
      }).toMap

      val resistancesMap = (node \ "resistances" \ "resistance").map(resNode => {
        val attackType = AttackType((resNode \ "@type").toString())
        val resistance = (resNode \ "@resistance").toString().toInt
        (attackType, resistance)
      }).toMap

      new SoldierType(name, cost, hp, movement, exp, level, attacks, moveCostsMap, defencesMap, resistancesMap, attributes)
    })

    parsed.toList
  }

  private def split(line: String) = if (line.isEmpty()) {
    Set()
  } else {
    line.split(",").map(_.trim()) toSet
  }
}

case class SoldierType(name: String, cost: Int, hp: Int, movement: Int, exp: Int, level: Int,
  attacks: List[Attack], moveCost: Map[TerrainType, Int], defence: Map[TerrainType, Int],
  resistance: Map[AttackType, Int], attributes: Set[SoldierTypeAttribute] = Set()) {

}