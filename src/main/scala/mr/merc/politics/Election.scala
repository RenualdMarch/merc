package mr.merc.politics

import mr.merc.economics.{EconomicRegion, Population}
import mr.merc.economics.MapUtil.FloatOperations._
import mr.merc.economics.Population.Culture

class Election(currentParty: Party, primaryCulture: Culture, possibleParties: List[Party]) {

  import Election._

  def doElections(regions: List[EconomicRegion]): StateElectionReport = {
    val reports = regions.map { r =>
      val reports = r.regionPopulation.pops.filter(p =>
        currentParty.votersPolicy.canVote(p.populationType, p.culture == primaryCulture)).
        map(_.choose(possibleParties))
      RegionElectionReport(r, reports)
    }
    StateElectionReport(reports)
  }
}

object Election {

  implicit class PopulationElection(population: Population) {

    def choose(parties: List[Party]): PopulationElectionReport = {
      val groups = population.politicalViews.currentViews(population.literacy).pointsOfView |*| population.populationCount
      val votesByGroups = groups.flatMap {case (position, count) =>
        val diff = parties.groupBy(p => position.diffWithPosition(p.politicalPosition))
        val minKey = diff.keySet.min
        val mostPopularParties = diff(minKey)
        val countPerParty = count / mostPopularParties.size
        mostPopularParties.map(_ -> countPerParty)
      }
      val votes = votesByGroups.groupBy(_._1).map { case (p, list) =>
          p -> list.values.sum
      }
      val zeros = (parties.toSet -- votes.keySet).map(_ -> 0d).toMap
      PopulationElectionReport(population, votes ++ zeros)
    }
  }

}

trait ElectionReport {
  def votes: Map[Party, Double]
}

case class StateElectionReport(reports: List[RegionElectionReport]) extends ElectionReport {
  override val votes: Map[Party, Double] = reports.foldLeft(Map[Party, Double]())(_ |+| _.votes)
}

case class RegionElectionReport(region: EconomicRegion, reports: List[PopulationElectionReport]) extends ElectionReport {
  override val votes: Map[Party, Double] = reports.foldLeft(Map[Party, Double]())(_ |+| _.votes)
}

case class PopulationElectionReport(population: Population, votes: Map[Party, Double]) extends ElectionReport