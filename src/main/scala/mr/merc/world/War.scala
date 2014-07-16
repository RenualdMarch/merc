package mr.merc.world

import mr.merc.map.world.Province
import mr.merc.map.world.WorldMap
import mr.merc.map.world.Province
import mr.merc.world.character.ComputerCharacter
import mr.merc.map.Grid
import mr.merc.map.pathfind.PathFinder
import mr.merc.world.character.HumanCharacter

class War(val firstSide: Country, val secondSide: Country, world: WorldMap) {
  val daysForStage = 30
  def firstSideProvinces = firstSide.provinces.toSet
  def secondSideProvinces = secondSide.provinces.toSet

  private def possibleAttacks(possibleAttackers: Set[Province], possibleDefenders: Set[Province]): List[AttackPlan] = {
    val possiblePaths = possibleAttackers.flatMap(p => world.provinceConnections(p).map(c => (p, c._1)))
    val possibleAttacks = possiblePaths.filter(pair => possibleDefenders.contains(pair._2))
    possibleAttacks map (pp => AttackPlan(pp._1, pp._2)) toList
  }

  def containsCountry = Set(firstSide, secondSide).contains _
  def otherSide(c: Country) = if (firstSide == c) secondSide
  else if (secondSide == c) firstSide
  else sys.error(s"$c is not taking part in war beetween $firstSide and $secondSide")
}

case class AttackPlan(from: Province, to: Province)