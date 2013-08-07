package mr.merc.map.hex

import mr.merc.map.terrain.TerrainType
import mr.merc.map.terrain.Hill
import mr.merc.map.terrain.Water
import mr.merc.map.terrain.BankOutside
import mr.merc.map.terrain.BankInside
import mr.merc.map.terrain.Mountain
import mr.merc.map.terrain.Forest
import mr.merc.map.terrain.Grass

object TerrainHexViewAdditive {
  def extractAdditives(view:TerrainHexView):List[TerrainHexViewAdditive] = {
    val neig = view.neighbours
    val different = neig.filter(n => n._2.terrain != view.hex.terrain)
    
    val additives = different.map(n => new TerrainHexViewAdditive(n._1, n._1, view.hex.terrain, n._2.terrain))
    val transformed = additives.flatMap(applyTerrainTypeCustomRules)
    uniteAdditives(transformed, view.hex.terrain)
   }
   
  private def uniteAdditives(tr:Traversable[TerrainHexViewAdditive], terrainType:TerrainType):List[TerrainHexViewAdditive] = {
    var retList = tr.toList
    while (whichCanBeUnited(retList).isDefined) {
      val toUnite = whichCanBeUnited(retList).get
      retList = retList.filterNot(p => p == toUnite._1 || p == toUnite._2)
      val united = unite(toUnite._1, toUnite._2, terrainType)
      retList ::= united      
    }
    
    retList
  }
  
  private def applyTerrainTypeCustomRules(add:TerrainHexViewAdditive):Option[TerrainHexViewAdditive] = {
    (add.hexTerrainType, add.neighbourTerrainType) match {
      case (Water, Water) => Some(add)
      case (Forest, _) => None
      case (_, Water) => Some(new TerrainHexViewAdditive(add.from, add.to, add.hexTerrainType, BankOutside))
      case (Water, _) => Some(new TerrainHexViewAdditive(add.from, add.to, Water, BankInside))
      case (Hill, _) => None
      case (_, Hill) => None
      case (_, Mountain) => None
      case (Mountain, _) => None      
      case (_, Forest) => Some(new TerrainHexViewAdditive(add.from, add.to, add.hexTerrainType, Grass))
      case (_, _) => Some(add)
    }
  }
  
  private def whichCanBeUnited(tr:Traversable[TerrainHexViewAdditive]):Option[(TerrainHexViewAdditive, TerrainHexViewAdditive)] = {
    tr.foreach(add1 => {
      tr.foreach(add2 => {
        if(add1 != add2 && canBeUnited(add1, add2)) {
          return Some((add1, add2))
        }
      })
    })
    
    None
  }
  
  private def canBeUnited(first:TerrainHexViewAdditive, second:TerrainHexViewAdditive):Boolean = {
    first.neighbourTerrainType == second.neighbourTerrainType && (Directions.isNeighbour(first.from, second.to) || Directions.isNeighbour(first.to, second.from))
  }
  
  private def unite(first:TerrainHexViewAdditive, second:TerrainHexViewAdditive, terrain:TerrainType):TerrainHexViewAdditive = {
    require(first.neighbourTerrainType == second.neighbourTerrainType)
    val pair = Directions.unite((first.from, first.to), (second.from, second.to))
    new TerrainHexViewAdditive(pair._1, pair._2, terrain, first.neighbourTerrainType)
  }
}

class TerrainHexViewAdditive(_from:Directions.Direction, 
    _to:Directions.Direction, val hexTerrainType:TerrainType, val neighbourTerrainType:TerrainType) {
  
  private val neighbours = Directions.neighbours(_from).contains(_to)
  private val pair:Directions.DirPair = {
    if (neighbours && Directions.next(_to) == _from) {
      (Directions.N, Directions.NW)
    } else {
      (_from, _to)
    }
    
  }
  
  val from = pair._1
  val to = pair._2
}