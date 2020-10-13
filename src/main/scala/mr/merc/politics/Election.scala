package mr.merc.politics

import mr.merc.economics.{EconomicRegion, Population}
import mr.merc.economics.MapUtil.FloatOperations._
import mr.merc.economics.MapUtil.NumericOperations.CollectionMapWithOperations
import mr.merc.economics.Culture

import scala.collection.concurrent.TrieMap

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

  def totalPopulationPopularity(regions: List[EconomicRegion]): StateElectionReport = {
    val reports = regions.map { r =>
      val reports = r.regionPopulation.pops.map(_.choose(Party.allParties))
      RegionElectionReport(r, reports)
    }
    StateElectionReport(reports)
  }
}

object Election {

  def mostPopularParty(pops: List[Population], allowedParties: List[Party]): Party = {
    pops.par.map(_.choose(allowedParties)).map(_.votes).reduce(_ |+| _)
    }.maxBy(_._2)._1

  implicit class PopulationElection(population: Population) {

    def choose(parties: List[Party]): PopulationElectionReport = {
      val groups = population.politicalViews.currentViews(population.literacy).pointsOfView |*| population.populationCount
      val votesByGroups = groups.map { case (position, count) =>
        val mostPopularParties = PopulationElection.mostPopularParties(position, parties)
        val countPerParty = count / mostPopularParties.size
        mostPopularParties.map(_ -> countPerParty).toMap
      }.sumAll
      val votes = votesByGroups.groupBy(_._1).map { case (p, list) =>
        p -> list.values.sum
      }
      val zeros = (parties.toSet -- votes.keySet).map(_ -> 0d).toMap
      PopulationElectionReport(population, votes ++ zeros)
    }
  }

  object PopulationElection {
    private val cache = new TrieMap[(PoliticalPosition, List[Party]), List[Party]]()

    def mostPopularParties(politicalPosition: PoliticalPosition, parties: List[Party]): List[Party] = {
      cache.getOrElseUpdate(politicalPosition -> parties, {
        val diff = parties.groupBy(p => politicalPosition.diffWithPosition(p.politicalPosition))
        val minKey = diff.keySet.min
        diff(minKey)
      })
    }
  }

}

trait ElectionReport {
  def votes: Map[Party, Double]
}

case class StateElectionReport(reports: List[RegionElectionReport]) extends ElectionReport {

  override val votes: Map[Party, Double] = reports.foldLeft(Map[Party, Double]())(_ |+| _.votes)

  def riggedElections(party: Party, additionalPart: Double): StateElectionReport = {
    def rigReport(r: RegionElectionReport): RegionElectionReport = {
      val popReports = r.reports.map { popReport =>
        val allVotes = popReport.votes.values.sum
        val newVotes = allVotes * additionalPart
        val withNew = popReport.votes |+| Map(party -> newVotes)
        PopulationElectionReport(popReport.population, withNew)
      }
      RegionElectionReport(r.region, popReports)
    }

    StateElectionReport(reports.map(rigReport))
  }
}

case class RegionElectionReport(region: EconomicRegion, reports: List[PopulationElectionReport]) extends ElectionReport {
  override val votes: Map[Party, Double] = reports.foldLeft(Map[Party, Double]())(_ |+| _.votes)
}

case class PopulationElectionReport(population: Population, votes: Map[Party, Double]) extends ElectionReport