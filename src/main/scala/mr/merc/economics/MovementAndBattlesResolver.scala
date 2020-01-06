package mr.merc.economics

import mr.merc.army.Warrior
import mr.merc.politics.Province

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

          new OneProvinceBattle(state.worldHexField.buildTerrainHexField(state.seasonOfYear.season), mv.to, additionalAttackers + (mv.from -> attackersSoldiers.toList),
            defendersSoldiers, additionalDefenders)
        }
      } else Nil
    }

    val twoProvinceBattles = for {
      first <- singleAttacks
      second <- singleAttacks if first.attackers.contains(second.province) && second.attackers.contains(first.province)
    } yield new TwoProvinceBattle(state.worldHexField.buildTerrainHexField(state.seasonOfYear.season), first.province, second.province, first.attackers,
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

