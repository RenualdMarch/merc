package mr.merc.map.hex

import mr.merc.map.terrain.TerrainType

object TerrainHexViewAdditive {
  def extractAdditives(view:TerrainHexView):List[TerrainHexViewAdditive] = {
    val neig = view.neighbours
    val different = neig.filter(n => n._2.terrain != view.hex.terrain)
    
    val additives = different.map(n => new TerrainHexViewAdditive(n._1, n._1, view.hex.terrain, n._2.terrain))
    uniteAdditives(additives, view.hex.terrain)
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

class TerrainHexViewAdditive(originalFrom:Directions.Direction, 
    originalTo:Directions.Direction, val hexTerrainType:TerrainType, val neighbourTerrainType:TerrainType) {

  private val normalized = Directions.normalizeClockwise(originalFrom, originalTo)
  val from = normalized._1
  val to = normalized._2
}