package mr.merc.economics

import mr.merc.economics.Population.{Aristocrats, Capitalists, Culture, Humans, Traders}
import mr.merc.map.objects.HumanVillageHouse
import mr.merc.politics._
import org.scalatest.FunSuite
import scalafx.scene.paint.Color

class ElectionTest extends FunSuite {
  val culture = new Culture("test", Humans, HumanVillageHouse, Color.White) {}
  val culture2 = new Culture("test2", Humans, HumanVillageHouse, Color.White) {}

  test("choose best party") {
    val bestParty = Party("best", Color.White,
      Migration.OpenBorders, Regime.Absolute, ForeignPolicy.Expansionism, Economy.StateEconomy,
      SocialPolicy.LifeNeedsSocialSecurity, VotersPolicy.PrimaryUpperClass)
    val worstParty = new Party("worst", Color.Black,
      Migration.ClosedBorders, Regime.Democracy, ForeignPolicy.Pacifism, Economy.FreeMarket,
      SocialPolicy.RegularNeedsSocialSecurity, VotersPolicy.NoVoting)
    val position = new PoliticalViews(
      Migration.popularity(1, 0, 1, 0),
      Regime.popularity(1, 0, 0, 1, 0, 0),
      ForeignPolicy.popularity(1, 0, 1, 0),
      Economy.popularity(1, 0, 0, 1, 0, 0),
      SocialPolicy.popularity(0, 1, 0, 0, 1, 0),
      VotersPolicy.popularity(0, 1, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0))
    val population = new Population(culture, Traders, 100, 0, 0, position)
    import Election.PopulationElection

    val report = population.choose(List(bestParty, worstParty))
    assert(report === PopulationElectionReport(population, Map(bestParty -> 100, worstParty -> 0)))
  }

  test("choose when there are two best parties") {
    val bestParty1 = Party("best", Color.White,
      Migration.OpenBorders, Regime.Absolute, ForeignPolicy.Expansionism, Economy.StateEconomy,
      SocialPolicy.NoSocialSecurity, VotersPolicy.PrimaryUpperClass)
    val bestParty2 = Party("best", Color.White,
      Migration.OpenBorders, Regime.Absolute, ForeignPolicy.Expansionism, Economy.StateEconomy,
      SocialPolicy.RegularNeedsSocialSecurity, VotersPolicy.PrimaryUpperClass)
    val worstParty = new Party("worst", Color.Black,
      Migration.ClosedBorders, Regime.Democracy, ForeignPolicy.Pacifism, Economy.FreeMarket,
      SocialPolicy.RegularNeedsSocialSecurity, VotersPolicy.NoVoting)

    val position = new PoliticalViews(
      Migration.popularity(1, 0, 1, 0),
      Regime.popularity(1, 0, 0, 1, 0, 0),
      ForeignPolicy.popularity(1, 0, 1, 0),
      Economy.popularity(1, 0, 0, 1, 0, 0),
      SocialPolicy.popularity(0, 1, 0, 0, 1, 0),
      VotersPolicy.popularity(0, 1, 0, 0, 0, 0,
        0, 1, 0, 0, 0, 0))

    val population = new Population(culture, Traders, 100, 0, 0, position)
    import Election.PopulationElection

    val report = population.choose(List(bestParty1, bestParty2, worstParty))
    assert(report === PopulationElectionReport(population, Map(bestParty1 -> 50, bestParty2 -> 50, worstParty -> 0)))
  }

  test("real election in one region") {
    val party1 = Party("best1", Color.White,
      Migration.OpenBorders, Regime.Absolute, ForeignPolicy.Expansionism, Economy.StateEconomy,
      SocialPolicy.NoSocialSecurity, VotersPolicy.PrimaryUpperClass)
    val party2 = Party("best2", Color.White,
      Migration.OpenBorders, Regime.Absolute, ForeignPolicy.Expansionism, Economy.StateEconomy,
      SocialPolicy.RegularNeedsSocialSecurity, VotersPolicy.PrimaryUpperClass)
    val party3 = new Party("worst", Color.Black,
      Migration.ClosedBorders, Regime.Democracy, ForeignPolicy.Pacifism, Economy.FreeMarket,
      SocialPolicy.RegularNeedsSocialSecurity, VotersPolicy.NoVoting)


    val position = new PoliticalViews(
      Migration.popularity(1, 0, 0, 1),
      Regime.popularity(1, 0, 0, 1, 0, 0),
      ForeignPolicy.popularity(1, 0, 0, 1),
      Economy.popularity(1, 0, 0, 0, 0, 1),
      SocialPolicy.popularity(1, 0, 0, 0, 1, 0),
      VotersPolicy.popularity(0, 1, 0, 0, 0, 0,
        0, 0, 0, 1, 0, 0))

    val population1 = new Population(culture, Aristocrats, 100, 0, 0, position)
    val population2 = new Population(culture, Capitalists, 150, 0, 150, position)
    val population3 = new Population(culture, Traders, 15000, 0, 150, position)
    val population4 = new Population(culture2, Traders, 15000, 0, 150, position)


    val rp = new RegionPopulation(List(population1, population2, population3, population4))
    val region = new EconomicRegion {
      override def owner: State = ???

      override def economicNeighbours: Set[EconomicRegion] = ???

      override val regionMarket: RegionMarket = null
      override val regionPopulation: RegionPopulation = rp
    }

    val election = new Election(party1, culture, List(party1, party2, party3))
    val report = election.doElections(List(region))
    assert(report === StateElectionReport(List(RegionElectionReport(region, List(
      PopulationElectionReport(population1, Map(party1 -> 100, party2 -> 0, party3 -> 0)),
      PopulationElectionReport(population2, Map(party3 -> 150, party1 -> 0, party2 -> 0))
    )))))

    assert(report.votes === Map(party1 -> 100, party3 -> 150, party2 -> 0))
  }

}
