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
      val positions = for {
        m <- Migration.possiblePositions
        r <- Regime.possiblePositions
        fp <- ForeignPolicy.possiblePositions
        e <- Economy.possiblePositions
        sp <- SocialPolicy.possiblePositions
        vp <- VotersPolicy.possiblePositions
      } yield PoliticalPosition(m, r, fp, e, sp, vp)
      val groups = positions.map { p =>
        val lit = population.literacy
        val pv = population.politicalViews
        val groupCount = population.populationCount *
          pv.migration.popularity(lit)(p.migration) *
          pv.regime.popularity(lit)(p.regime) *
          pv.foreignPolicy.popularity(lit)(p.foreignPolicy) *
          pv.economy.popularity(lit)(p.economy) *
          pv.socialPolicy.popularity(lit)(p.socialPolicy) *
          pv.votersPolicy.popularity(lit)(p.votersPolicy)
        (p, groupCount)
      }
      val votesByGroups = groups.flatMap {case (position, count) =>
        val diff = parties.groupBy(p => position.diffWithPosition(p.politicalPosition))
        val minKey = diff.keySet.min
        val mostPopularParties = diff(minKey)
        val countPerParty = count / mostPopularParties.size
        mostPopularParties.map(_ -> countPerParty)
      }
      val votes = votesByGroups.groupBy(_._1).map { case (p, list) =>
          p -> list.map(_._2).sum
      }
      PopulationElectionReport(population, votes)
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