package mr.merc.economics

import mr.merc.politics.{Party, PopulationElectionReport, RegionElectionReport, State, StateElectionReport}
import org.scalatest.FunSuite
import Party._

class PoliticalSystemTest extends FunSuite {

  private val state = new State("", Culture.LatinHuman, 0, Party.absolute, 0)

  private def electionResults(map:Map[Party, Double]) = StateElectionReport(List(RegionElectionReport(null,
    List(PopulationElectionReport(null, map)))))

  test("findCoalition") {
    val politicalSystem = new PoliticalSystem(conservative, state, 0)
    val coalition1 = politicalSystem.findCoalition(Map(aristocratic -> 0.51, benevolent -> 0.4, capitalistic -> 0.09))
    assert(coalition1 === Set(aristocratic))

    val coalition2 = politicalSystem.findCoalition(Map(aristocratic -> 0.2, benevolent -> 0.4, socialDemocratic -> 0.4))
    assert(coalition2 === Set(socialDemocratic, benevolent))
  }

  test("applyElectionResults") {
    val politicalSystem = new PoliticalSystem(conservative, state, 0)
    politicalSystem.applyElectionResults(electionResults(Map(
        manufactorers -> 300,
        capitalistic -> 400,
        theocratic -> 300
      )))
    assert(politicalSystem.rulingParty === capitalistic)
    assert(politicalSystem.parliament === Some(ParliamentParties(Map(
      manufactorers -> 0.3, capitalistic -> 0.4, theocratic -> 0.3),
      Set(manufactorers, capitalistic))))
  }

  test("changeAbsoluteRulingParty") {
    val politicalSystem = new PoliticalSystem(absolute, state, 0)
    politicalSystem.changeAbsoluteRulingParty(benevolent)
    assert(politicalSystem.rulingParty === benevolent)
    assert(politicalSystem.parliament === None)

    intercept[RuntimeException] {
      val ps2 = new PoliticalSystem(conservative, state, 0)
      ps2.changeAbsoluteRulingParty(benevolent)
    }

    intercept[RuntimeException] {
      politicalSystem.changeAbsoluteRulingParty(capitalistic)
    }
  }

  test("usurpPower to absolute") {
    val politicalSystem = new PoliticalSystem(capitalistic, state, 0)
    politicalSystem.usurpPower(benevolent)
    assert(politicalSystem.parliament === None)
    assert(politicalSystem.rulingParty === benevolent)

    val ps2 = new PoliticalSystem(capitalistic, state, 0)
    intercept[RuntimeException] {
      ps2.usurpPower(magocratic)
    }
  }

  test("usurpPower to constitutional") {
    val politicalSystem = new PoliticalSystem(conservative, state, 0)
    politicalSystem.usurpPower(magocratic)
    assert(politicalSystem.parliament.isDefined === true)
    assert(politicalSystem.parliament.get.coalition === Set(magocratic))
    assert(politicalSystem.parliament.get.parties(magocratic) > 0.5)
    assert(politicalSystem.rulingParty === magocratic)


    intercept[RuntimeException] {
      val ps2 = new PoliticalSystem(conservative, state, 0)
      ps2.usurpPower(absolute)
    }
  }

  test("giveUpPower to constitutional") {
    val politicalSystem = new PoliticalSystem(benevolent, state, 0)
    politicalSystem.giveUpPower(magocratic, 2)
    assert(politicalSystem.parliament.isDefined === true)
    assert(politicalSystem.parliament.get.coalition === Set(magocratic))
    assert(politicalSystem.parliament.get.parties ===Map(magocratic -> 1d))
    assert(politicalSystem.rulingParty === magocratic)

    intercept[RuntimeException] {
      val ps2 = new PoliticalSystem(benevolent, state, 0)
      ps2.giveUpPower(conservative, 2)
    }

  }

  test("giveUpPower to democracy") {
    val politicalSystem = new PoliticalSystem(magocratic, state, 0)
    politicalSystem.giveUpPower(conservative, 2)
    assert(politicalSystem.parliament.isDefined === true)
    assert(politicalSystem.parliament.get.coalition === Set(conservative))
    assert(politicalSystem.parliament.get.parties(conservative) > 0.5)
    assert(politicalSystem.rulingParty === conservative)

    intercept[RuntimeException] {
      val ps2 = new PoliticalSystem(magocratic, state, 0)
      ps2.giveUpPower(benevolent,2)
    }
  }

  test("new political system") {
    val ps = new PoliticalSystem(absolute, state, 0)
    assert(ps.parliament === None)
    assert(ps.rulingParty === absolute)

    val ps2 = new PoliticalSystem(magocratic, state, 0)
    assert(ps2.parliament === Some(ParliamentParties(Map(magocratic -> 1d), Set(magocratic))))
  }
}
