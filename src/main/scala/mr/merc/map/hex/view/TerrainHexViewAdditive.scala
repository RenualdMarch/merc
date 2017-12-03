package mr.merc.map.hex.view

import mr.merc.map.terrain._
import mr.merc.map.hex.Direction
import mr.merc.map.hex.N
import mr.merc.map.hex.NW


object TerrainHexViewAdditive {
  def extractAdditives(view: TerrainHexView): List[TerrainHexViewAdditive] = {
    val neig = view.neighbours
    val different = neig.filter(n => n._2.terrain != view.hex.terrain)

    val additives = different.map(n => new TerrainHexViewAdditive(n._1, n._1, view.hex.terrain, n._2.terrain))
    val transformed = additives.flatMap(applyTerrainTypeCustomRules)
    uniteAdditives(transformed, view.hex.terrain)
  }

  private def uniteAdditives(tr: Traversable[TerrainHexViewAdditive], terrainType: TerrainType): List[TerrainHexViewAdditive] = {
    var retList = tr.toList
    while (whichCanBeUnited(retList).isDefined) {
      val toUnite = whichCanBeUnited(retList).get
      retList = retList.filterNot(p => p == toUnite._1 || p == toUnite._2)
      val united = unite(toUnite._1, toUnite._2, terrainType)
      retList ::= united
    }

    retList
  }

  private def applyTerrainTypeCustomRules(add: TerrainHexViewAdditive): Option[TerrainHexViewAdditive] = {
    (add.hexTerrainType, add.neighbourTerrainType) match {
      case (Empty, _) | (_, Empty)=> None
      case (Water, Water) => Some(add)
      case (Forest, _) => None
      case (_, Water) => Some(new TerrainHexViewAdditive(add.from, add.to, add.hexTerrainType, BankOutside))
      case (Water, _) => Some(new TerrainHexViewAdditive(add.from, add.to, Water, BankInside))
      case (Mountain, _) => None
      case (_, Forest) => Some(new TerrainHexViewAdditive(add.from, add.to, add.hexTerrainType, Grass))
      case (Castle, _) | (_, Castle) => None
      case (_, _) => Some(add)
    }
  }

  private def whichCanBeUnited(tr: Traversable[TerrainHexViewAdditive]): Option[(TerrainHexViewAdditive, TerrainHexViewAdditive)] = {
    tr.foreach(add1 => {
      tr.foreach(add2 => {
        if (add1 != add2 && canBeUnited(add1, add2)) {
          return Some((add1, add2))
        }
      })
    })

    None
  }

  private def canBeUnited(first: TerrainHexViewAdditive, second: TerrainHexViewAdditive): Boolean = {
    first.neighbourTerrainType == second.neighbourTerrainType && (first.from.isNeighbour(second.to) || first.to.isNeighbour(second.from))
  }

  private def unite(first: TerrainHexViewAdditive, second: TerrainHexViewAdditive, terrain: TerrainType): TerrainHexViewAdditive = {
    require(first.neighbourTerrainType == second.neighbourTerrainType)
    val pair = Direction.unite((first.from, first.to), (second.from, second.to))
    new TerrainHexViewAdditive(pair._1, pair._2, terrain, first.neighbourTerrainType)
  }
}

class TerrainHexViewAdditive(_from: Direction, _to: Direction, val hexTerrainType: TerrainType, val neighbourTerrainType: TerrainType) {

  private val neighbours = _from.neighbours.contains(_to)
  private val pair: Direction.DirPair = {
    if (neighbours && _to.next == _from) {
      (N, NW)
    } else {
      (_from, _to)
    }

  }

  val from = pair._1
  val to = pair._2
}