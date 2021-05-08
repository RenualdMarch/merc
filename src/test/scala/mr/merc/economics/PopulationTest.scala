package mr.merc.economics

import mr.merc.army.WarriorViewNames
import mr.merc.economics.Population._
import Culture._
import mr.merc.economics.Culture.CultureAlignment.{ColorAlignment, PriorityAlignment}
import mr.merc.economics.Products.{Coal, Grain, Liquor}
import mr.merc.economics.TaxPolicy.MiddleSalaryTax
import mr.merc.politics._
import org.scalatest.{FunSuite, Matchers}
import scalafx.scene.paint.Color

import scala.util.Random

class PopulationTest extends FunSuite with Matchers {

  val smallNeeds:Map[PopulationNeedsType, Map[Products.Product, Double]] = Map(
    LifeNeeds -> Map(Grain -> 2.0, Coal -> 1.0),
    RegularNeeds -> Map(Grain -> 3.0, Coal -> 2.0),
    LuxuryNeeds -> Map(Liquor -> 3.0))

  case object TestRace extends Race(16, 100, 0, 100)
  object TestCulture extends Culture("test",TestRace, "testHouse", Color.Black) {
    override def cultureAlignment: Culture.CultureAlignment = CultureAlignment(ColorAlignment.Gray, PriorityAlignment.Balanced)

    override val warriorViewNames: WarriorViewNames = LatinHuman.warriorViewNames
    override val cultureInfo: Culture.CultureInfo = LatinHuman.cultureInfo

    private val map: CornerPopulationNeeds = Map(Upper -> smallNeeds, Middle -> smallNeeds, Lower -> smallNeeds)

    override def needs: PopulationNeeds = PopulationNeeds(map, map)
  }
  def newPopulation(money: Double) = new Population(TestCulture, Traders, 1000, money, 0, PoliticalViews.averagePoliticalViews)

  test("demands calculation") {
    def assertDemands(money: Double, prices:Map[Products.Product, Double], demands:Map[Products.Product, Double]) {
      val pop = newPopulation(money)
      assert(pop.calculateDemands(prices) === demands)
    }

    val prices1 = Map[Products.Product, Double](Grain -> 1.0, Liquor -> 2.0)
    val prices2 = Map[Products.Product, Double](Grain -> 2.0, Liquor -> 1.0)
    val prices3 = Map[Products.Product, Double](Grain -> 1.0, Liquor -> 2.0, Coal -> 0.5)

    assertDemands(0, prices1, Map())
    assertDemands(500, prices1, Map(Grain -> 500))
    assertDemands(1500, prices1, Map(Grain -> 1500))
    assertDemands(5000, prices1, Map(Grain -> 5000))
    assertDemands(6000, prices1, Map(Grain -> 5000, Liquor -> 500))
    assertDemands(11000, prices1, Map(Grain -> 5000, Liquor -> 3000))
    assertDemands(12000, prices1, Map(Grain -> 5000, Liquor -> 3000))

    assertDemands(5000, prices2, Map(Grain -> 2500))
    assertDemands(10000, prices2, Map(Grain -> 5000))
    assertDemands(11000, prices2, Map(Grain -> 5000, Liquor -> 1000))

    assertDemands(500, prices3, Map(Coal -> 1000))
    assertDemands(1000, prices3, Map(Coal -> 1000, Grain -> 500))
    assertDemands(2500, prices3, Map(Coal -> 1000, Grain -> 2000))
    assertDemands(3000, prices3, Map(Coal -> 2000, Grain -> 2000))
    assertDemands(3500, prices3, Map(Coal -> 3000, Grain -> 2000))
    assertDemands(4500, prices3, Map(Coal -> 3000, Grain -> 3000))
    assertDemands(6500, prices3, Map(Coal -> 3000, Grain -> 5000))
    assertDemands(7500, prices3, Map(Coal -> 3000, Grain -> 5000, Liquor -> 500))
  }

  test("fulfill demands") {
    val pop1 = newPopulation(15000)

    pop1.buyDemandedProducts(List(FulfilledDemandRequest(5000, 1, PopulationDemandRequest(pop1, Grain, 5000)),
      FulfilledDemandRequest(3000, 2, PopulationDemandRequest(pop1, Liquor, 3000)), FulfilledDemandRequest(3000, 0.5,
        PopulationDemandRequest(pop1, Coal, 3000))))

    assert(pop1.moneyReserves === 2500)

    pop1.fulfillNeedsUsingAlreadyReceivedProducts()
    assert(pop1.currentDayRecord.productFulfillment.needsFulfillment === Map(LifeNeeds -> 1, RegularNeeds -> 1, LuxuryNeeds -> 1))

    val pop2 = newPopulation(1000)
   pop2.buyDemandedProducts(List(FulfilledDemandRequest(0, 1, PopulationDemandRequest(pop2, Grain, 1000)),
      FulfilledDemandRequest(0, 2, PopulationDemandRequest(pop2, Liquor, 1000)),
      FulfilledDemandRequest(0, 0.5, PopulationDemandRequest(pop2, Coal, 1000))))

    assert(pop2.moneyReserves === 1000)
    pop2.fulfillNeedsUsingAlreadyReceivedProducts()
    assert(pop2.currentDayRecord.productFulfillment.needsFulfillment === Map(LifeNeeds -> 0, RegularNeeds -> 0, LuxuryNeeds -> 0))


    val pop3 = newPopulation(15000)
    pop3.buyDemandedProducts(List(FulfilledDemandRequest(500, 1,  PopulationDemandRequest(pop3, Grain, 5000)),
      FulfilledDemandRequest(1500, 2, PopulationDemandRequest(pop3, Liquor, 1500)),
      FulfilledDemandRequest(2000, 0.5, PopulationDemandRequest(pop3, Coal, 3000))))

    assert(pop3.moneyReserves === 10500)
    pop3.fulfillNeedsUsingAlreadyReceivedProducts()
    assert(pop3.currentDayRecord.productFulfillment.needsFulfillment === Map(LifeNeeds -> 0.5, RegularNeeds -> 0.2, LuxuryNeeds -> 0.5))
  }

  test("salary") {
    val pop = newPopulation(1000)
    pop.newDay(new TaxPolicy(Map(MiddleSalaryTax -> 0.1)), 1)
    pop.receiveSalary(10000)
    assert(pop.moneyReserves === 11000)
    pop.endOfDay()
    assert(pop.salary.head.totalMoney === 10000)
    pop.salary.head.taxes shouldBe 1000d +- 0.00001
  }

  test("extraction of people") {
    def newPop() = new Population(TestCulture, Traders, 1000, 0, 100, PoliticalViews.averagePoliticalViews)

    val p1 = newPop()
    val m1 = p1.extractRandomMovers(11)
    assert(p1.populationCount === 989)
    assert(p1.literateCount === 99)
    assert(m1.literateCount === 1)
    assert(m1.illiterateCount === 10)
    assert(m1.totalCount === 11)

    val p2 = newPop()
    val m2 = p2.extractLiterateMovers(10)
    assert(p2.populationCount === 990)
    assert(p2.literateCount === 90)
    assert(m2.literateCount === 10)
    assert(m2.illiterateCount === 0)
    assert(m2.totalCount === 10)

    val p3 = newPop()
    val m3 = p3.extractIlliterateMovers(10)
    assert(p3.populationCount === 990)
    assert(p3.literateCount === 100)
    assert(m3.literateCount === 0)
    assert(m3.illiterateCount === 10)
    assert(m3.totalCount === 10)

    val p4 = newPop()
    val m4 = p4.extractLiterateThenIlliterate(110)
    assert(p4.populationCount === 890)
    assert(p4.literateCount === 0)
    assert(m4.literateCount === 100)
    assert(m4.illiterateCount === 10)
    assert(m4.totalCount === 110)

    val p5 = newPop()
    val m5 = p5.extractLiterateThenIlliterate(1100)
    assert(p5.populationCount === 0)
    assert(p5.literateCount === 0)
    assert(m5.literateCount === 100)
    assert(m5.illiterateCount === 900)
    assert(m5.totalCount === 1000)

    val p6 = newPop()
    val m6 = p6.extractIlliterateThenLiterate(950)
    assert(p6.populationCount === 50)
    assert(p6.literateCount === 50)
    assert(m6.literateCount === 50)
    assert(m6.illiterateCount === 900)
    assert(m6.totalCount === 950)

    val p7 = newPop()
    val m7 = p7.extractLiterateThenIlliterate(10)
    assert(p7.populationCount === 990)
    assert(p7.literateCount === 90)
    assert(m7.literateCount === 10)
    assert(m7.illiterateCount === 0)
    assert(m7.totalCount === 10)

    val p8 = newPop()
    val m8 = p8.extractLiterateThenIlliterate(10)
    assert(p8.populationCount === 990)
    assert(p8.literateCount === 90)
    assert(m8.literateCount === 10)
    assert(m8.illiterateCount === 0)
    assert(m8.totalCount === 10)
  }

  test("no promotion or demotion when nowhere to promote or demote") {
    val random = new Random(0) {
      override def nextInt(n: Int): Int = 0
    }

    val pop = new Population(TestCulture, Traders, 1000, 10000, 100, PoliticalViews.averagePoliticalViews)
    val regionPopulation = new RegionPopulation(List(pop))


    pop.buyDemandedProducts(List(FulfilledDemandRequest(6000, 1, PopulationDemandRequest(pop, Grain, 6000)),
      FulfilledDemandRequest(4000, 2, PopulationDemandRequest(pop, Liquor, 4000)), FulfilledDemandRequest(4000, 0.5,
        PopulationDemandRequest(pop, Coal, 4000))))

    pop.fulfillNeedsUsingAlreadyReceivedProducts()
    regionPopulation.popsList.foreach(_.endOfDay())
    assert(pop.currentDayRecord.productFulfillment.needsFulfillment === Map(LifeNeeds -> 1, RegularNeeds -> 1, LuxuryNeeds -> 1))

    val ppd = new PopulationMigrationInsideProvince(regionPopulation, new State("1", TestCulture, 0, Party.aristocratic, 0))
    ppd.migrateInsideProvince()
    val traders = regionPopulation.pop(Traders, TestCulture)
    assert(traders.populationCount === 1000)
  }

  test("literacy learning") {
    val pop = new Population(TestCulture, Traders, 2000, 100000, 0, PoliticalViews.averagePoliticalViews)
    val pop2 = new Population(TestCulture, Farmers, 1000, 100000, 0, PoliticalViews.averagePoliticalViews)
    val pop3 = new Population(TestCulture, Scholars, 1000, 100000, 0, PoliticalViews.averagePoliticalViews)

    val regionPop = new RegionPopulation(List(pop, pop2, pop3))
    regionPop.learnLiteracy()

    List(pop, pop2, pop3).foreach {p =>
      p.literateCount should be > 0
    }

    pop2.literateCount shouldBe pop3.literateCount
    pop.literateCount should be >= pop2.literateCount * 2
  }

  test("political happiness test") {
    val absoluteRadical = PoliticalViews(Migration.popularity(0, 1, 1, 1),
      Regime.popularity(1, 0, 0, 1, 1, 1),
      ForeignPolicy.popularity(1, 0, 1, 1),
      Economy.popularity(1, 0, 1, 1),
      SocialPolicy.popularity(1, 0, 0, 1, 1, 1),
      VotersPolicy.popularity(1, 0, 0, 0, 0, 0,1,1,1,1,1,1))

    val pop = new Population(TestCulture, Traders, 2000, 100000, 0, absoluteRadical)
    val pop2 = new Population(LatinHuman, Traders, 2000, 100000, 0, absoluteRadical)

    val absoluteRadicalParty = new Party("", Color.White,
      Migration.ClosedBorders, Regime.Absolute, ForeignPolicy.Expansionism,
      Economy.StateEconomy, SocialPolicy.NoSocialSecurity, VotersPolicy.NoVoting)
    val state = new State("", TestCulture, 0, absoluteRadicalParty, 0)

    pop.politicalHappiness(state) shouldBe 1d
    pop2.politicalHappiness(state) shouldBe (1d - WorldConstants.Population.DifferentCulturePoliticalHappinessPenalty)

    val radicallyDifferentParty = new Party("", Color.Black,
      Migration.OpenBorders, Regime.Democracy, ForeignPolicy.Pacifism,
      Economy.FreeMarket, SocialPolicy.RegularNeedsSocialSecurity, VotersPolicy.Everyone)
    val differentState = new State("", TestCulture, 0, radicallyDifferentParty, 0)

    pop.politicalHappiness(differentState) shouldBe 0d
    pop2.politicalHappiness(differentState) shouldBe 0d
  }

  test("growth test") {
    val pop1 = new Population(TestCulture, Traders, 1000, 15000, 0, PoliticalViews.averagePoliticalViews)

    pop1.buyDemandedProducts(List(FulfilledDemandRequest(5000, 1, PopulationDemandRequest(pop1, Grain, 5000)),
      FulfilledDemandRequest(3000, 2, PopulationDemandRequest(pop1, Liquor, 3000)), FulfilledDemandRequest(3000, 0.5,
        PopulationDemandRequest(pop1, Coal, 3000))))

    pop1.fulfillNeedsUsingAlreadyReceivedProducts()
    assert(pop1.currentDayRecord.productFulfillment.needsFulfillment === Map(LifeNeeds -> 1, RegularNeeds -> 1, LuxuryNeeds -> 1))

    val pop2 = new Population(TestCulture, Traders, 1000, 1000, 100, PoliticalViews.averagePoliticalViews)
    pop2.buyDemandedProducts(List(FulfilledDemandRequest(0, 1, PopulationDemandRequest(pop2, Grain, 1000)),
      FulfilledDemandRequest(0, 2, PopulationDemandRequest(pop2, Liquor, 1000)),
      FulfilledDemandRequest(0, 0.5, PopulationDemandRequest(pop2, Coal, 1000))))
    pop2.fulfillNeedsUsingAlreadyReceivedProducts()
    assert(pop2.currentDayRecord.productFulfillment.needsFulfillment === Map(LifeNeeds -> 0, RegularNeeds -> 0, LuxuryNeeds -> 0))

    pop1.endOfDay()
    pop2.endOfDay()
    pop1.grow()
    pop2.grow()

    pop1.populationCount should be > 1000
    pop2.populationCount should be > 1000
    pop2.literateCount shouldBe 100
    pop1.populationCount should be > pop2.populationCount

  }
}
