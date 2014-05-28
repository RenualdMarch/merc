package mr.merc.world.mission

import mr.merc.world.WorldState

class MissionGenerator(worldState: WorldState) {
  private def wars = worldState.diplomacyEngine.currentWars

  def generateMissions(numberPerProvince: Double): List[Mission] = {
    ???
  }
}