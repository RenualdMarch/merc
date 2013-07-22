package mr.merc.map.hex

import mr.merc.map.terrain.TerrainType

object TerrainHexViewAdditive {
  def extractAdditives(view:TerrainHexView):List[TerrainHexViewAdditive] = {
    val neig = view.neighbours
    val different = neig.filter(n => n._2.terrain != view.hex.terrain)
    
    val additives = different.map(n => new TerrainHexViewAdditive(n._1, n._1, n._2.terrain))
    uniteAdditives(additives)
   }
  
  private def uniteAdditives(tr:Traversable[TerrainHexViewAdditive]):List[TerrainHexViewAdditive] = {
    var retList = tr.toList
    while (whichCanBeUnited(retList).isDefined) {
      val toUnite = whichCanBeUnited(retList).get
      retList = retList.filterNot(p => p == toUnite._1 || p == toUnite._2)
      val united = unite(toUnite._1, toUnite._2)
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
    first.terrainType == second.terrainType && (isNeighbour(first.from, second.to) || isNeighbour(first.to, second.from))
  }
  
  private def isNeighbour(first:Directions.Direction, second:Directions.Direction) = Directions.neighbours(first).contains(second)
  
  private def unite(first:TerrainHexViewAdditive, second:TerrainHexViewAdditive):TerrainHexViewAdditive = {
    require(first.terrainType == second.terrainType)
    if (isNeighbour(first.from, second.to)) {
      new TerrainHexViewAdditive(second.from, first.to, first.terrainType)
    } else if (isNeighbour(second.from, first.to)) {
      new TerrainHexViewAdditive(first.from, second.to, first.terrainType)
    } else {
      throw new IllegalAccessException("Additives are not neighbours")
    }
  }
}

class TerrainHexViewAdditive(originalFrom:Directions.Direction, 
    originalTo:Directions.Direction, val terrainType:TerrainType) {

  private val normalized = Directions.normalizeClockwise(originalFrom, originalTo)
  val from = normalized._1
  val to = normalized._2
}