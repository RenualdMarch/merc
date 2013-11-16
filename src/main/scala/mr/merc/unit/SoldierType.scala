package mr.merc.unit

import mr.merc.map.terrain.TerrainType
import scala.xml.XML
import java.io.File

object SoldierType {
  val list = parseTypes
  private val map = list.map(st => (st.name, st)).toMap
  
  def apply(name:String) = map(name)
  
  private def parseTypes:List[SoldierType] = {
    val path = getClass.getResource("/conf/soldierTypes.xml").toURI
    val xml = XML.loadFile(new File(path))
    val parsed = (xml \ "soldierType").map(node => {
      val name = (node \ "@name").toString()
      val cost = (node \ "@cost").toString().toInt
      val hp = (node \ "@hp").toString().toInt
      val movement = (node \ "@movement").toString().toInt
      val exp = (node \ "@exp").toString().toInt
      val level = (node \ "@level").toString().toInt

      val attacks = (node \ "attacks" \ "attack").map(attackNode => {
        val imageName = (attackNode \ "@imageName").toString()
        val damage = (attackNode \ "@damage").toString().toInt
        val count = (attackNode \ "@count").toString().toInt
        val ranged = (attackNode \ "@ranged").toString().toBoolean
        val attackTypeName = (attackNode \ "@attackType").toString()
        val attackType = AttackType(attackTypeName)
        new Attack(imageName, damage, count, attackType, ranged)
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
      
      new SoldierType(name, cost, hp, movement, exp, level, attacks, moveCostsMap, defencesMap, resistancesMap)
    })
    
    parsed.toList
	}
}

case class SoldierType(name:String, cost:Int, hp:Int, movement:Int, exp:Int, level:Int, 
    attacks:List[Attack], moveCost:Map[TerrainType, Int], defence:Map[TerrainType, Int], 
    resistance:Map[AttackType, Int], soldierTypeAttributes:Set[SoldierAttribute] = Set()) {
  
  
}