package mr.merc.world.mission

import mr.merc.world.WorldState
import scala.util.Random
import mr.merc.world.Country
import mr.merc.map.world.Province
import mr.merc.util.MercUtils._

class MissionGenerator(worldState: WorldState) {
  private def wars = worldState.diplomacyEngine.currentWars

  def enemies(c: Country) = wars.filter(_.containsCountry(c)).map(_.otherSide(c))

  def generateMissions(province: Province, min: Int, max: Int): List[Mission] = {
    val missionsNumber = min + Random.nextInt(max - min)

    val ctx = MissionContext(worldState, province)
    val missions: List[Mission] = 0 until missionsNumber flatMap { _ =>
      val generators = MissionGenerators.missionGenerators.values
      val generator = generators.randomElement
      generator(ctx)
    } toList

    missions.map(x => (x.comparingObj, x)).toMap.values.toList
  }

  private def borderingEnemies(province: Province): List[(Province, Country)] = {
    val map = worldState.map
    val country = map.countryByProvince(province)
    val neigs = map.neighbours(province).map(n => (n, map.countryByProvince(n)))
    val currentEnemies = enemies(country)
    neigs filter (x => currentEnemies.contains(x._2)) toList
  }
}