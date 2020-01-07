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

    def kindBelowOfThis(tt: TerrainType): TerrainKind = {
      tt.belowTerrainType.map(_.kind).getOrElse(tt.kind)
    }

    (kindBelowOfThis(add.hexTerrainType), kindBelowOfThis(add.neighbourTerrainType)) match {
      case (EmptyKind, _) | (_, EmptyKind)=> None
      case (WaterKind, WaterKind) => Some(add)
      case (IceKind, WaterKind) | (WaterKind, IceKind) => Some(add)
      case (_, WaterKind) => Some(new TerrainHexViewAdditive(add.from, add.to, add.hexTerrainType, BankOutside))
      case (WaterKind, _) => Some(new TerrainHexViewAdditive(add.from, add.to, add.hexTerrainType, BankInside))
      case (MountainKind, _) => None
      case (WallsKind, _) | (_, WallsKind) => None
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
    first.neighbourTerrainType == second.neighbourTerrainType &&
      DirectionsRange(first.from, first.to).canBeUnited(DirectionsRange(second.from, second.to))
  }

  private def unite(first: TerrainHexViewAdditive, second: TerrainHexViewAdditive, terrain: TerrainType): TerrainHexViewAdditive = {
    require(first.neighbourTerrainType == second.neighbourTerrainType)
    val dr1 = DirectionsRange(first.from, first.to)
    val dr2 = DirectionsRange(second.from, second.to)
    require(dr1.canBeUnited(dr2), s"Cann't unite $dr1 and $dr2")

    val (from, to) = (dr1 + dr2).toDirPair.get
    new TerrainHexViewAdditive(from, to, terrain, first.neighbourTerrainType)
  }
}

case class TerrainHexViewAdditive(from: Direction, to: Direction, hexTerrainType: TerrainType, neighbourTerrainType: TerrainType) {

  private val neighbours = from.neighbours.contains(to)
  private val pair: Direction.DirPair = {
    if (neighbours && to.next == from) {
      (N, NW)
    } else {
      (from, to)
    }

  }
}