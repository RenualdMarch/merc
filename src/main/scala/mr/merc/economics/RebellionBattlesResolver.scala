package mr.merc.economics

import mr.merc.economics.RegionPopulation.Rebellion
import mr.merc.politics.Election

class RebellionBattlesResolver(state: WorldState) {

  def rebellions(rebellions:List[Rebellion]):Map[Rebellion, Battle] = {
    rebellions.map { reb =>
      val mostPopularParty = Election.mostPopularParty(reb.pops, state.possibleParties(reb.province.owner.politicalSystem))
      val newRebelState = state.generateNewState(reb.rebellionCulture, mostPopularParty, 0, reb.province.name)
      reb -> new RebellionOneProvinceBattle(state.worldHexField.buildTerrainHexField(state.seasonOfYear.season),
        reb.province, newRebelState, reb.province.regionWarriors.allWarriors)
    }.toMap
  }
}
