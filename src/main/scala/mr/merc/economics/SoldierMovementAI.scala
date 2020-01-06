package mr.merc.economics

import mr.merc.army.Warrior
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement
import mr.merc.log.Logging
import mr.merc.map.UniversalGrid
import mr.merc.map.pathfind.PathFinder
import mr.merc.politics.{Province, State}

class SoldierMovementAI(worldState: WorldState, state: State) extends Logging{
  private val advantageToAttack = 2d

  private val stateRegions = worldState.regions.filter(_.owner == state)
  private val allSoldiers = worldState.regions.flatMap(_.regionWarriors.allWarriors.filter(_.owner == state))
  private val warriorRegions = worldState.regions.filter(_.regionWarriors.allWarriors.exists(_.owner == state))

  def orderSoldiers(): Unit = {
    // TODO - fill later
  }

  def moveSoldiers(): Unit = {
    debug(s"${state.name} in moving soldiers now")
    val grid = new UniversalGrid[Province] {
      override def heuristic(from: Province, to: Province): Double = 0

      override def cellWhereMovementMustBeStopped(t: Province): Boolean = false

      override def cellWhereItIsForbiddenToStop(t: Province): Boolean = false

      override def isBlocked(t: Province): Boolean = !worldState.canAccessProvince(state, t)

      override def price(from: Province, to: Province): Double = 1

      override def neighbours(t: Province): List[Province] = t.neighbours
    }

    worldState.diplomacyEngine.wars(state) match {
      case Nil =>
        debug(s"${state.name} in not in wars")
        returnSoldiersHome(grid)
        divideSoldiersEquallyAmongProvinces()
      case wars =>
        debug(s"${state.name} is in war")
        moveWarriorsToBorderProvinces(wars, grid)
        attackIfAdvantage(wars)
    }
  }

  def moveWarriorsToBorderProvinces(wars:List[WarAgreement], grid: UniversalGrid[Province]):Unit = {
    val enemies = wars.flatMap(_.oppositeSideByState(state)).toSet
    val allies = wars.flatMap(_.sideByState(state)).toSet
    val enemyRegions = worldState.regions.filter(r => enemies.contains(r.controller)).toSet
    val allyRegions = worldState.regions.filter(r => allies.contains(r.controller))

    val allyBorderRegions = allyRegions.filter(p => (enemyRegions & p.neighbours.toSet).nonEmpty)

    warriorRegions.foreach {p =>
      val possiblePaths = PathFinder.findPossiblePaths(grid, p)
      val borders = allyBorderRegions.map(border => border -> possiblePaths.get(border)).collect {
        case (border, Some(list)) => border -> list
      }
      if (borders.nonEmpty) {
        borders.minBy(_._2.size)._2.tail.headOption.foreach { target =>
          val stateSoldiers = p.regionWarriors.allWarriors.filter(_.owner == state)
          debug(s"In ${p.name} plans move $stateSoldiers to province ${target.name}")
          worldState.planMoveArmy(p, Some(target), stateSoldiers)
        }
      }
    }
  }

  def attackIfAdvantage(wars:List[WarAgreement]): Unit = {
    val enemies = wars.flatMap(_.oppositeSideByState(state)).toSet
    val allies = wars.flatMap(_.sideByState(state)).toSet
    val enemyRegions = worldState.regions.filter(r => enemies.contains(r.controller)).toSet
    val allyRegions = worldState.regions.filter(r => allies.contains(r.controller))

    val allyBorderRegions = allyRegions.filter(p => (enemyRegions & p.neighbours.toSet).nonEmpty)
    allyBorderRegions.foreach {p =>
      val allyWarriors = p.regionWarriors.allWarriors.filter(w => allies.contains(w.owner))
      val allySum = allyWarriors.map(_.warriorWeight).sum
      val provinceToAttack = p.neighbours.filter(r => enemies.contains(r.controller)).find(p =>
        (p.regionPopulation.populationCount / WorldConstants.Population.HousePerPopulation + p.regionWarriors.allWarriors.filter(w => enemies.contains(w.owner)).map(_.warriorWeight).sum) * advantageToAttack < allySum)
      val stateWarriors = allyWarriors.filter(_.owner == state)
      if (stateWarriors.nonEmpty) {
        provinceToAttack.foreach { pa =>
          debug(s"In ${p.name} plans move $stateWarriors to province ${pa.name}")
          worldState.planMoveArmy(p, Some(pa), stateWarriors)
        }
      }
    }
  }

  // algorithm sends soldiers to provinces that are not full
  def divideSoldiersEquallyAmongProvinces(): Unit = {
    val levelSumPerProvince = Math.ceil(allSoldiers.map(_.warriorWeight).sum.toDouble / stateRegions.size).toInt

    val warriorsSum = stateRegions.map { r =>
      r -> r.regionWarriors.allWarriors.filter(_.owner == state).map(_.warriorWeight).sum
    }.toMap

    stateRegions.filter(r => warriorsSum(r) > levelSumPerProvince).foreach { r =>
      val warriors = r.regionWarriors.allWarriors.filter(_.owner == state)
      val ew = excessiveWarriors(levelSumPerProvince, warriors)
      val neigs = r.neighbours.filter(_.owner == state)
      neigs match {
        case Nil => // do nothing
          debug(s"No plans for ${r.name}")
        case List(one) =>
          debug(s"In ${r.name} plans move $ew to province ${one.name}")
          worldState.planMoveArmy(r, Some(one), ew)
        case others =>
          val regionsToSend = others.filter(r => warriorsSum(r) < levelSumPerProvince)
          if (regionsToSend.nonEmpty) {
            regionsToSend.zip(divideWarriors(ew, regionsToSend.size)).foreach { case (n, list) =>
              debug(s"In ${r.name} plans move $list to province ${n.name}")
              worldState.planMoveArmy(r, Some(n), list)
            }
          }
      }
    }
  }

  def divideWarriors(warriors:List[Warrior], parts: Int):List[List[Warrior]] = {
    val size = Math.ceil(warriors.size.toDouble / parts).toInt
    warriors.grouped(size).toList
  }

  def excessiveWarriors(levelsPerProvince:Int, warriors:List[Warrior]): List[Warrior] = {
    var sum = 0d
    warriors.dropWhile { w =>
      sum += w.warriorWeight
      sum < levelsPerProvince
    }
  }

  def returnSoldiersHome(grid: UniversalGrid[Province]): Unit = {
    val foreignWarriors = warriorRegions.toSet &~ stateRegions.toSet

    foreignWarriors.foreach { p =>
      val possible = PathFinder.findPossiblePaths(grid, p)
      val pathsToStateRegions = stateRegions.map(sr => sr -> possible.get(sr)).collect {
        case (k, Some(v)) => k -> v
      }

      if (pathsToStateRegions.nonEmpty) {
        val warriors = p.regionWarriors.allWarriors.filter(_.owner == state)

        pathsToStateRegions.minBy(_._2.size)._2.tail.headOption.foreach { target =>
          worldState.planMoveArmy(p, Some(target), warriors)
        }
      }
    }
  }
}
