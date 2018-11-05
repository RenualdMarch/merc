package mr.merc.map.hex.view

import mr.merc.map.terrain._
import mr.merc.map.hex._


object TerrainHexViewAdditiveRule {
  // first is drawn first
  private def orderOfTypes = List[TerrainKind](MountainKind, WaterKind, HillKind, ForestKind, SwampKind, RoadKind, GrassKind, SandKind)
}

/**
  * Rule decides which elements should be drawn
  */
class TerrainHexViewAdditiveRule {
  def transform(add: Traversable[TerrainHexViewAdditive]): List[TerrainHexViewAdditiveElement] = {
    val filtered = filterNotNeededAdditives(add)
    filtered.flatMap(additivesToElements)
  }

  private[hex] def filterNotNeededAdditives(add: Traversable[TerrainHexViewAdditive]): List[TerrainHexViewAdditive] = {
    add.filter { viewAdd =>
      if (TerrainType.helperTypesList.contains(viewAdd.hexTerrainType) || TerrainType.helperTypesList.contains(viewAdd.neighbourTerrainType)) {
        true
      } else {
        val strengthOfCurrent = TerrainHexViewAdditiveRule.orderOfTypes.indexOf(viewAdd.hexTerrainType.kind)
        val strengthOfNeighbour = TerrainHexViewAdditiveRule.orderOfTypes.indexOf(viewAdd.neighbourTerrainType.kind)
        if (strengthOfCurrent != strengthOfNeighbour) {
          strengthOfCurrent > strengthOfNeighbour
        } else {
          // Some non-random ordering
          viewAdd.hexTerrainType.getClass.getName > viewAdd.neighbourTerrainType.getClass.getName
        }
      }
    }.toList
  }

  private[hex] def additivesToElements(add: TerrainHexViewAdditive): List[TerrainHexViewAdditiveElement] = {
    val allElements = TerrainHexViewAdditiveElement.elements(add.neighbourTerrainType)
    val possibleElements = allElements.filter(e => additiveContainsElement(add, e))
    selectElements(add, possibleElements)
  }

  private def selectElements(add: TerrainHexViewAdditive, possible: List[TerrainHexViewAdditiveElement]): List[TerrainHexViewAdditiveElement] = {
    case class Answer(parent:Option[Answer], selected: TerrainHexViewAdditiveElement) {
      val allElements:List[TerrainHexViewAdditiveElement] = {
        parent match {
          case None => selected :: Nil
          case Some(p) => selected :: p.allElements
        }
      }

      val isValid:Boolean = {
        val listOfRanges = allElements.map(el => DirectionsRange.apply(el.from, el.to))
        val sumOfRangesSizes = listOfRanges.map(_.size).sum
        val rangeOfSum = listOfRanges.reduce(_ + _)
        sumOfRangesSizes == rangeOfSum.size && sumOfRangesSizes <= DirectionsRange(add.from, add.to).size
      }

      val isAnswer:Boolean = isValid && {
        val sum = allElements.map(el => DirectionsRange.apply(el.from, el.to)).reduce(_ + _)
        val target = DirectionsRange(add.from, add.to)
        sum == target
      }

      def findSolutions():List[List[TerrainHexViewAdditiveElement]] = {
        if (isAnswer) List(allElements)
        else if (!isValid) Nil
        else {
          possible.flatMap(Answer(Some(this), _).findSolutions())
        }
      }
    }

    possible.flatMap(Answer(None, _).findSolutions()).sortBy(_.size).headOption.getOrElse(
      sys.error(s"Couldn't select elements for additive $add with possibilities $possible"))
  }

  private def additiveContainsElement(add: TerrainHexViewAdditive, elem: TerrainHexViewAdditiveElement): Boolean = {
    DirectionsRange(add.from, add.to).contains(DirectionsRange(elem.from, elem.to))
  }
}
