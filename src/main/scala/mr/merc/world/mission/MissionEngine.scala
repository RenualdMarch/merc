package mr.merc.world.mission

import mr.merc.map.world.Province
import mr.merc.world.WorldCalendar
import mr.merc.world.WorldState

class MissionEngine(worldState: WorldState) {
  private val generator = new MissionGenerator(worldState)
  private val maxMissions = 4
  private val daysToRenewMissions = 60
  private val activeMissions: scala.collection.mutable.Map[Province, List[Mission]] = scala.collection.mutable.Map()

  generateMissions()

  def updateMissions() {
    // TODO if n days passed then recreate missions
    ???
  }

  private def generateMissions() {
    activeMissions.clear()
    worldState.map.provinces.foreach { p =>
      val missions = generator.generateMissions(p, 0, maxMissions)
      activeMissions += p -> missions
    }
  }
}