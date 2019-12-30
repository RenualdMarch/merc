package mr.merc.economics

import mr.merc.army.WarriorType.HeavyMaceInfantry
import mr.merc.army.{Warrior, WarriorViewNames}
import mr.merc.economics.Population.Humans
import mr.merc.politics.{Party, State}
import org.scalatest.{FunSuite, Matchers}
import scalafx.scene.paint.Color
import MapUtil.FloatOperations._
import mr.merc.army.WarriorCompetence.{Militia, Professional}
import mr.merc.economics.Culture.{CultureInfo, StateForm}

class RegionWarriorsTest extends FunSuite with Matchers {

  val testCulture = new Culture("testCulture", Humans, "testHouse", Color.White) {
    override val warriorViewNames: WarriorViewNames = WarriorViewNames(Map(
      (HeavyMaceInfantry, Professional) -> "oneImageSoldier",
      (HeavyMaceInfantry, Militia) -> "testSoldier2"))
    override val cultureInfo: Culture.CultureInfo = CultureInfo(StateForm("a", "b"), Nil, Nil)
  }

  val state = new State("", testCulture, 0, new PoliticalSystem(Party.absolute))

  val region1 = new EconomicRegion {
    override def owner: State = ???

    override def economicNeighbours: Set[EconomicRegion] = ???

    override val regionMarket: RegionMarket = null
    override val regionPopulation: RegionPopulation = null
    override val regionWarriors: RegionWarriors = new RegionWarriors(Nil, Set(r2))
  }

  val region2 = new EconomicRegion {
    override def owner: State = ???

    override def economicNeighbours: Set[EconomicRegion] = ???

    override val regionMarket: RegionMarket = null
    override val regionPopulation: RegionPopulation = null
    override val regionWarriors: RegionWarriors = new RegionWarriors(Nil, Set(r1))
  }

  def r1:EconomicRegion = region1
  def r2:EconomicRegion = region2

  test("initial") {
    val w1 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)
    val w2 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)

    val regionWarriors = new RegionWarriors(List(w1, w2), Set(region1, region2))
    regionWarriors.warriorDestinations shouldBe Map(None -> List(w1, w2))
  }

  test("plans") {
    val w1 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)
    val w2 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)

    val regionWarriors = new RegionWarriors(List(w1, w2), Set(region1, region2))
    regionWarriors.planSendWarriors(List(w1), Some(region2))
    regionWarriors.warriorDestinations shouldBe Map(None -> List(w2), Some(region2) -> List(w1))
    regionWarriors.planSendWarriors(List(w2), Some(region2))
    regionWarriors.warriorDestinations shouldBe Map(None -> Nil, Some(region2) -> List(w1, w2))
  }

  test("invalid plans") {
    val w1 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)
    val w2 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)

    val regionWarriors = new RegionWarriors(List(w1, w2), Set(region1, region2))

    val region3 = new EconomicRegion {
      override def owner: State = ???

      override def economicNeighbours: Set[EconomicRegion] = ???

      override val regionMarket: RegionMarket = null
      override val regionPopulation: RegionPopulation = null
      override val regionWarriors: RegionWarriors = new RegionWarriors(Nil, Set())
    }

    val ex = the [RuntimeException] thrownBy {
      regionWarriors.planSendWarriors(List(w1, w2), Some(region3))
    }
    ex.getMessage should include ("Illegal destination")
  }

  test("move soldiers") {
    val w1 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)
    val w2 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)

    val regionWarriors = new RegionWarriors(List(w1, w2), Set(region1, region2))
    regionWarriors.planSendWarriors(List(w1), Some(region1))
    val map = regionWarriors.sendWarriorsToDestinations()
    regionWarriors.warriorDestinations shouldBe Map(None -> List(w2))
    map shouldBe Map(region1 -> List(w1))
  }

  test("receive soldiers") {
    val w1 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)
    val w2 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)

    val regionWarriors = new RegionWarriors(List(w1), Set(region1, region2))
    regionWarriors.receiveWarriors(List(w2))
    regionWarriors.warriorDestinations shouldBe Map(None -> List(w1, w2))
  }

  test("take soldiers") {
    val w1 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)
    val w2 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)

    val regionWarriors = new RegionWarriors(List(w1, w2), Set(region1, region2))
    val taken = regionWarriors.takeWarriors(List(w1))
    regionWarriors.warriorDestinations shouldBe Map(None -> List(w2))
    taken shouldBe List(w1)
    val taken2 = regionWarriors.takeWarriors(List(w1))
    taken2 shouldBe Nil
  }

  test("clear dead soldiers") {
    val w1 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)
    val w2 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)

    val regionWarriors = new RegionWarriors(List(w1, w2), Set(region1, region2))

    w1.hpPercentage = 0
    val dead = regionWarriors.clearDeadWarriors()
    dead shouldBe List(w1)
    regionWarriors.warriorDestinations shouldBe Map(None -> List(w2))
  }

  test("generate demands when money are present") {
    val state = new State("", testCulture, 100000, new PoliticalSystem(Party.absolute))

    val w1 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)
    val w2 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)

    val region3 = new EconomicRegion {
      override def owner: State = state

      override def economicNeighbours: Set[EconomicRegion] = ???

      override val regionMarket: RegionMarket = new RegionMarket(Map().withDefaultValue(1d))
      override val regionPopulation: RegionPopulation = null
      override val regionWarriors: RegionWarriors = new RegionWarriors(List(w1, w2), Set())
    }

    val armyNeeds = region3.regionWarriors.generateArmyNeeds()
    val totalNeeds = armyNeeds.map(an => Map(an.product -> an.count)).reduce(_ |+| _)
    totalNeeds shouldBe w1.needs |+| w2.needs
  }

  test("no demands when money are absent") {
    val state = new State("", testCulture, 0, new PoliticalSystem(Party.absolute))

    val w1 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)
    val w2 = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)

    val region3 = new EconomicRegion {
      override def owner: State = state

      override def economicNeighbours: Set[EconomicRegion] = ???

      override val regionMarket: RegionMarket = new RegionMarket(Map().withDefaultValue(1d))
      override val regionPopulation: RegionPopulation = null
      override val regionWarriors: RegionWarriors = new RegionWarriors(List(w1, w2), Set())
    }

    val armyNeeds = region3.regionWarriors.generateArmyNeeds()
    armyNeeds should have size 0
  }
}
