package mr.merc.economics

import mr.merc.army.{Warrior, WarriorCompetence, WarriorType}
import mr.merc.map.hex.{TerrainHex, TerrainHexField}
import mr.merc.map.objects.House
import mr.merc.map.terrain.{Empty, TerrainType}
import mr.merc.politics.{Province, State}
import mr.merc.unit.Soldier
import mr.merc.army.WarriorCompetence._
import mr.merc.map.GameField
import mr.merc.util.MercUtils._

class MovementAndBattlesResolver(state: WorldState) {

  def moveAndPrepareBattles():List[Battle] = {
    val allMovers = extractAllMovers()
    val battles = calculateBattles(allMovers)
    val (successful, failed) = successfulAndFailedMovers(allMovers, battles)
    moveSuccessfullMovers(successful)
    returnFailedMoversBack(failed)
    battles
  }

  def extractAllMovers():List[MovementFromTo] = {
    state.regions.flatMap { region =>
      region.regionWarriors.sendWarriorsToDestinations().map { case (p, list) =>
        MovementFromTo(region, p.asInstanceOf[Province], list)
      }
    }
  }

  def moveSuccessfullMovers(movers:List[MovementFromTo]): Unit ={
    movers.foreach { mv =>
      mv.to.regionWarriors.receiveWarriors(mv.warriors)
    }
  }

  def successfulAndFailedMovers(allMovers:List[MovementFromTo], battles: List[Battle]):(List[MovementFromTo], List[MovementFromTo]) = {
    val provinceBattleMap = battles.flatMap { battle =>
      battle.provinces.map(_ -> battle)
    }.toMap

    val succAndFail = allMovers.map { case MovementFromTo(from, to, warriors) =>
        provinceBattleMap.get(to) match {
          case None =>
            val correctWarriors = warriors.groupBy(_.owner).filter{
              case (owner, _) => state.canPlanMoveArmy(from, to, owner)}.flatMap(_._2).toList
            val failedWarriors = warriors diff correctWarriors
            (MovementFromTo(from, to, correctWarriors), MovementFromTo(from, to, failedWarriors))
          case Some(battle) =>
            val failedWarriors = warriors diff battle.allWarriors
            (MovementFromTo(from, to, Nil), MovementFromTo(from, to, failedWarriors))
        }
    }

    val succ = succAndFail.map(_._1).filter(_.warriors.nonEmpty)
    val fail = succAndFail.map(_._2).filter(_.warriors.nonEmpty)

    (succ, fail)
  }

  def calculateBattles(movers:List[MovementFromTo]):List[Battle] = {
    val possible = allPossibleBattles(movers)
    selectBattles(possible)
  }

  def allPossibleBattles(movers:List[MovementFromTo]):List[Battle] = {
    val singleAttacks:List[OneProvinceBattle] = movers.flatMap { mv =>
      val attackers = mv.warriors.groupBy(_.owner).filter {
        case (attackersOwner, _) => state.inWarAgainst(attackersOwner, mv.to.controller)
      }
      if (attackers.nonEmpty) {
        val attackersSet = state.mergeAttackersTogether(attackers.keySet, mv.to.controller).toList
        attackersSet.map { owners =>
          val attackersSoldiers = owners.flatMap(s => attackers(s))
          val defendersSoldiers = mv.to.regionWarriors.allWarriors.groupBy(_.owner).collect {
            case (d, list) if owners.exists(s => state.inWarAgainst(s, d)) => list
          }.flatten.toList

          val additionalAttackers = movers.filter(add => add.from != mv.from && add.to == mv.to).map { add =>
            val warriors = add.warriors.groupBy(_.owner).filter{ case (ow, _) =>
              owners.exists(s => state.inWarTogetherAgainst(s, ow, mv.to.controller))
            }.flatMap(_._2)
            (add.from, warriors.toList)
          }.filter(_._2.nonEmpty).toMap

          val additionalDefenders = movers.filter(add => add.from != mv.from && add.to == mv.to).map { add =>
            val warriors = add.warriors.groupBy(_.owner).filter{ case (ow, _) =>
              owners.exists(at => state.inWarAgainst(ow, at))
            }.flatMap(_._2)
            (add.from, warriors.toList)
          }.filter(_._2.nonEmpty).toMap

          new OneProvinceBattle(mv.to, additionalAttackers + (mv.from -> attackersSoldiers.toList),
            defendersSoldiers, additionalDefenders)
        }
      } else Nil
    }

    val twoProvinceBattles = for {
      first <- singleAttacks
      second <- singleAttacks if first.attackers.contains(second.province) && second.attackers.contains(first.province)
    } yield new TwoProvinceBattle(first.province, second.province, first.attackers,
      first.defenders, first.additionalDefenders, second.attackers, second.defenders,
      second.additionalDefenders)

    singleAttacks ::: twoProvinceBattles
  }

  def selectBattles(battles:List[Battle]):List[Battle] = {
    def intersections(current:List[Battle]):List[(Battle, Battle)] = {
      for {
        (b1, idx1) <- current.zipWithIndex
        (b2, idx2) <- current.zipWithIndex if idx1 < idx2 && b1.intersect(b2)
      } yield (b1, b2)
    }

    val toRemove = intersections(battles).map {
      case (b1, b2) =>
        if (b1.battleWeight > b2.battleWeight) b2
        else b1
    }

    battles diff toRemove
  }

  def returnFailedMoversBack(failedMovers:List[MovementFromTo]): Unit = {
    failedMovers.foreach { mv =>
      mv.from.regionWarriors.receiveWarriors(mv.warriors)
    }
  }

}

case class MovementFromTo(from:Province, to:Province, warriors:List[Warrior])

abstract sealed class Battle() {

  val allWarriors:List[Warrior]

  lazy val battleWeight:Double = allWarriors.map(_.warriorWeight).sum

  def intersect(battle: Battle): Boolean = provinces.intersect(battle.provinces).nonEmpty

  def provinces:List[Province]

  def additionalProvinces:List[Province]

  def placement:Map[Province, List[Warrior]]

  def putWarriors(hexField: TerrainHexField): Unit = {
    putMilitiaSoldiers(hexField)
    placement.foreach { case (province, list) =>
      if (provinces.contains(province)) {
        putWarriorsToProvince(list, province, hexField)
      } else if (additionalProvinces.contains(province)) {
        putWarriorsToAdditionalProvince(list, province, hexField)
      } else sys.error(s"province $province doesn't belong to provinces $provinces or additional $additionalProvinces")
    }
  }

  def putMilitiaSoldiers(hexField: TerrainHexField): Unit = {
    def randomMilitiaSoldier(culture: Culture, owner:State):Soldier = {
      val selectedType = culture.warriorViewNames.possibleWarriors.collect{
        case ((wt, wc), _) if wc == WarriorCompetence.Militia => wt}.randomElement()
      new Warrior(selectedType, Militia, culture, owner).soldier
    }

    for {
      hex <- hexField.hexes
      house <- hex.mapObj.collect{case hs:House => hs}
    } {
      hex.province match {
        case Some(p) =>
          if (p.owner == p.controller) {
            hex.soldier = Some(randomMilitiaSoldier(house.culture, p.owner))
          }
        case None => sys.error(s"Province must be present in hex $hex in hexField $hexField")
      }
    }
  }

  def putWarriorsToProvince(warriors:List[Warrior], province: Province, hexField: TerrainHexField): Unit = {
    val (x, y) = hexField.hexes.filter(_.province.contains(province)).map(h => (h.x, h.y)).medianBySquares
    hexField.closest(hexField.hex(x, y)).filter(h => h.soldier.isEmpty).zip(warriors).foreach { case (hex, warrior: Warrior) =>
      hex.soldier = Some(warrior.soldier)
    }
  }

  def putWarriorsToAdditionalProvince(warriors:List[Warrior], province: Province, hexField: TerrainHexField): Unit ={
    val intersection = hexField.hexes.filter(_.province.contains(province)).toSet
    require(intersection.nonEmpty, s"province $province doesn't belong to hexField $hexField")

    hexField.closest(intersection).filter(_.soldier.isEmpty).zip(warriors).foreach { case (hex, warrior) =>
      hex.soldier = Some(warrior.soldier)
    }
  }

  def terrainHexFieldFromProvinces(worldHexField:TerrainHexField):TerrainHexField = {
    val provinceHexes = provinces.flatMap(_.hexes).toSet
    val neigs = provinceHexes.flatMap(p => worldHexField.neighboursSet(p))
    val additionalHexes = additionalProvinces.flatMap(_.hexes).toSet & neigs
    val allHexes = provinceHexes ++ additionalHexes

    val minX = allHexes.minBy(_.x).x
    val minY = allHexes.minBy(_.y).y
    val maxX = allHexes.maxBy(_.x).x
    val maxY = allHexes.maxBy(_.y).y

    val allHexesMap = allHexes.map(h => (h.x, h.y) -> h).toMap

    def copyField(x:Int, y:Int):TerrainHex = {
      allHexesMap.get(x + minX, y + minY) match {
        case Some(originalHex) =>
          val hex = new TerrainHex(x, y, originalHex.terrain)
          hex.mapObj = originalHex.mapObj
          hex.province = originalHex.province
          hex
        case None => new TerrainHex(x, y, Empty)
      }
    }

    new TerrainHexField(maxX - minX + 2, maxY - minY + 2, copyField)
  }

  def buildBattle(worldHexField:TerrainHexField):GameField = {
    val battleHexField = terrainHexFieldFromProvinces(worldHexField)
    putMilitiaSoldiers(battleHexField)
    putWarriors(battleHexField)
    val (first, second) = sides
    val allSides = Set(first.map(_.toPlayer), second.map(_.toPlayer))
    new GameField(battleHexField, allSides.flatten.toList, allSides)
  }

  def sides:(Set[State], Set[State])

}

class OneProvinceBattle(val province: Province, val attackers:Map[Province, List[Warrior]], val defenders:List[Warrior],
                        val additionalDefenders:Map[Province, List[Warrior]]) extends Battle {

  override val allWarriors: List[Warrior] = defenders ++ attackers.values.flatten ++ additionalDefenders.values.flatten

  override def provinces: List[Province] = List(province)

  def additionalProvinces:List[Province] = (attackers.keySet ++ additionalDefenders.keySet).toList

  override def placement: Map[Province, List[Warrior]] = {
    import cats.implicits._
    Map(province -> defenders) |+| attackers |+| additionalDefenders
  }

  private def attackersSet:Set[State] = attackers.values.flatten.map(_.owner).toSet

  private def defendersSet:Set[State] = defenders.map(_.owner).toSet ++ additionalDefenders.values.flatten.map(_.owner)

  override def sides: (Set[State], Set[State]) = (attackersSet, defendersSet)
}

class TwoProvinceBattle(val province1:Province, val province2:Province, val province1Attackers:Map[Province, List[Warrior]],
                        val province1Defenders:List[Warrior], val province1AdditionalDefenders:Map[Province, List[Warrior]],
                        val province2Attackers:Map[Province, List[Warrior]], val province2Defenders:List[Warrior],
                        val province2AdditionalDefenders:Map[Province, List[Warrior]]) extends Battle {

  override val allWarriors: List[Warrior] = province1Defenders ++ province1Attackers.values.flatten ++
    province1AdditionalDefenders.values.flatten ++ province2Attackers.values.flatten ++ province2Defenders ++
    province2AdditionalDefenders.values.flatten

  override def provinces: List[Province] = List(province1, province2)

  override lazy val battleWeight:Double = allWarriors.map(_.warriorWeight).sum +
    province1Attackers.getOrElse(province2, Nil).map(_.warriorWeight).sum +
    province2Attackers.getOrElse(province1, Nil).map(_.warriorWeight).sum

  def additionalProvinces:List[Province] = (province1Attackers.keySet ++ province2Attackers.keySet ++
    province1AdditionalDefenders.keySet ++ province2AdditionalDefenders.keySet).toList

  override def placement: Map[Province, List[Warrior]] = {
    import cats.implicits._
    Map(province1 -> province1Defenders, province2 -> province2Defenders) |+| province1Attackers |+|
      province2Attackers |+| province1AdditionalDefenders |+| province2AdditionalDefenders
  }

  private def attackers1Set:Set[State] = province2Defenders.map(_.owner).toSet ++
    province1Attackers.values.flatten.map(_.owner) ++ province2AdditionalDefenders.values.flatten.map(_.owner)

  private def attackers2Set:Set[State] = province1Defenders.map(_.owner).toSet ++
    province2Attackers.values.flatten.map(_.owner) ++ province1AdditionalDefenders.values.flatten.map(_.owner)

  override def sides: (Set[State], Set[State]) = (attackers1Set, attackers2Set)
}
