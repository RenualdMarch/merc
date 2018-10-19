package mr.merc.economics

import mr.merc.economics.Population._
import mr.merc.economics.Products.{Coal, Fruit, Grain}
import org.scalatest.FunSuite

import scala.util.Random

class PopulationTest extends FunSuite {

  val smallNeeds:Map[PopulationNeedsType, Map[Products.Product, Double]] = Map(
    LifeNeeds -> Map(Grain -> 2.0, Coal -> 1.0),
    RegularNeeds -> Map(Grain -> 3.0, Coal -> 2.0),
    LuxuryNeeds -> Map(Fruit -> 3.0))

  object TestRace extends Race
  object TestCulture extends Culture("test", TestRace) {
    override def needs: PopulationNeeds = Map(Upper -> smallNeeds, Middle -> smallNeeds, Lower -> smallNeeds)
  }
  def newPopulation(money: Double) = new Population(TestCulture, Traders, 1000, money, 0)

  test("demands calculation") {
    def assertDemands(money: Double, prices:Map[Products.Product, Double], demands:Map[Products.Product, Double]) {
      val pop = newPopulation(money)
      assert(pop.calculateDemands(prices) === demands)
    }

    val prices1 = Map[Products.Product, Double](Grain -> 1.0, Fruit -> 2.0)
    val prices2 = Map[Products.Product, Double](Grain -> 2.0, Fruit -> 1.0)
    val prices3 = Map[Products.Product, Double](Grain -> 1.0, Fruit -> 2.0, Coal -> 0.5)

    assertDemands(0, prices1, Map())
    assertDemands(500, prices1, Map(Grain -> 500))
    assertDemands(1500, prices1, Map(Grain -> 1500))
    assertDemands(5000, prices1, Map(Grain -> 5000))
    assertDemands(6000, prices1, Map(Grain -> 5000, Fruit -> 500))
    assertDemands(11000, prices1, Map(Grain -> 5000, Fruit -> 3000))
    assertDemands(12000, prices1, Map(Grain -> 5000, Fruit -> 3000))

    assertDemands(5000, prices2, Map(Grain -> 2500))
    assertDemands(10000, prices2, Map(Grain -> 5000))
    assertDemands(11000, prices2, Map(Grain -> 5000, Fruit -> 1000))

    assertDemands(500, prices3, Map(Coal -> 1000))
    assertDemands(1000, prices3, Map(Coal -> 1000, Grain -> 500))
    assertDemands(2500, prices3, Map(Coal -> 1000, Grain -> 2000))
    assertDemands(3000, prices3, Map(Coal -> 2000, Grain -> 2000))
    assertDemands(3500, prices3, Map(Coal -> 3000, Grain -> 2000))
    assertDemands(4500, prices3, Map(Coal -> 3000, Grain -> 3000))
    assertDemands(6500, prices3, Map(Coal -> 3000, Grain -> 5000))
    assertDemands(7500, prices3, Map(Coal -> 3000, Grain -> 5000, Fruit -> 500))
  }

  test("fulfill demands") {
    val pop1 = newPopulation(15000)

    val price1 = pop1.buyDemandedProducts(List(FulfilledDemandRequest(5000, 1, PopulationDemandRequest(pop1, Grain, 5000)),
      FulfilledDemandRequest(3000, 2, PopulationDemandRequest(pop1, Fruit, 3000)), FulfilledDemandRequest(3000, 0.5,
        PopulationDemandRequest(pop1, Coal, 3000))))

    assert(price1 === 12500)

    assert(pop1.moneyReserves === 2500)

    pop1.fulfillNeedsUsingAlreadyReceivedProducts()
    assert(pop1.needsFulfillment(1).head.needsFulfillment === Map(LifeNeeds -> 1, RegularNeeds -> 1, LuxuryNeeds -> 1))

    val pop2 = newPopulation(1000)
    val price2 = pop2.buyDemandedProducts(List(FulfilledDemandRequest(0, 1, PopulationDemandRequest(pop2, Grain, 1000)),
      FulfilledDemandRequest(0, 2, PopulationDemandRequest(pop2, Fruit, 1000)),
      FulfilledDemandRequest(0, 0.5, PopulationDemandRequest(pop2, Coal, 1000))))

    assert(price2 === 0)
    assert(pop2.moneyReserves === 1000)
    pop2.fulfillNeedsUsingAlreadyReceivedProducts()
    assert(pop2.needsFulfillment(1).head.needsFulfillment === Map(LifeNeeds -> 0, RegularNeeds -> 0, LuxuryNeeds -> 0))


    val pop3 = newPopulation(15000)
    val price3 = pop3.buyDemandedProducts(List(FulfilledDemandRequest(500, 1,  PopulationDemandRequest(pop3, Grain, 5000)),
      FulfilledDemandRequest(1500, 2, PopulationDemandRequest(pop3, Fruit, 1500)),
      FulfilledDemandRequest(2000, 0.5, PopulationDemandRequest(pop3, Coal, 3000))))

    assert(price3 === 4500)

    assert(pop3.moneyReserves === 10500)
    pop3.fulfillNeedsUsingAlreadyReceivedProducts()
    assert(pop3.needsFulfillment(1).head.needsFulfillment === Map(LifeNeeds -> 0.5, RegularNeeds -> 0.2, LuxuryNeeds -> 0.5))
  }

  test("salary") {
    val pop = newPopulation(1000)
    pop.newDay(SalaryTaxPolicy(Map(Middle -> 0)))
    pop.receiveSalary(10000)
    assert(pop.moneyReserves === 11000)
    pop.newDay(SalaryTaxPolicy(Map(Middle -> 0)))
    assert(pop.salary(1).head.totalMoney === 11000)
  }

  test("extraction of people") {
    def newPop() = new Population(TestCulture, Traders, 1000, 0, 100)

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

  test("promotion") {
    val random = new Random(0) {
      override def nextInt(n: Int): Int = 0
    }

    val pop = new Population(TestCulture, Traders, 1000, 10000, 100)
    val pop2 = new Population(TestCulture, Capitalists, 10, 1000, 10)
    val regionPopulation = new RegionPopulation(List(pop, pop2))


    pop.buyDemandedProducts(List(FulfilledDemandRequest(6000, 1, PopulationDemandRequest(pop, Grain, 6000)),
      FulfilledDemandRequest(4000, 2, PopulationDemandRequest(pop, Fruit, 4000)), FulfilledDemandRequest(4000, 0.5,
        PopulationDemandRequest(pop, Coal, 4000))))


    pop2.buyDemandedProducts(List(FulfilledDemandRequest(600, 1, PopulationDemandRequest(pop2, Grain, 600)),
      FulfilledDemandRequest(400, 2, PopulationDemandRequest(pop2, Fruit, 400)), FulfilledDemandRequest(400, 0.5,
        PopulationDemandRequest(pop2, Coal, 400))))

    pop.fulfillNeedsUsingAlreadyReceivedProducts()
    pop2.fulfillNeedsUsingAlreadyReceivedProducts()

    assert(pop.needsFulfillment(1).head.needsFulfillment === Map(LifeNeeds -> 1, RegularNeeds -> 1, LuxuryNeeds -> 1))
    assert(pop2.needsFulfillment(1).head.needsFulfillment === Map(LifeNeeds -> 1, RegularNeeds -> 1, LuxuryNeeds -> 1))


    PopulationPromotionDemotion.promoteOrDemote(regionPopulation, random)
    val traders = regionPopulation.pop(Traders, TestCulture)
    assert(traders.populationCount === 965)
    assert(traders.literateCount === 70)

    // promotion from traders
    val capitalists = regionPopulation.pop(Capitalists, TestCulture)
    assert(capitalists.populationCount === 39)
    assert(capitalists.literateCount === 39)

    // movement from capitalists
    val aristocrats = regionPopulation.pop(Aristocrats, TestCulture)
    assert(aristocrats.populationCount === 1)
    assert(aristocrats.literateCount === 1)

    // random movement from traders
    val craftsmen = regionPopulation.pop(Craftsmen, TestCulture)
    assert(craftsmen.populationCount === 5)
    assert(craftsmen.literateCount === 0)
  }

  test("no promotion") {
    val random = new Random(0) {
      override def nextInt(n: Int): Int = 0
    }

    val pop = new Population(TestCulture, Traders, 1000, 10000, 100)
    val pop2 = new Population(TestCulture, Capitalists, 10, 0, 10)
    val regionPopulation = new RegionPopulation(List(pop, pop2))


    pop.buyDemandedProducts(List(FulfilledDemandRequest(6000, 1, PopulationDemandRequest(pop, Grain, 6000)),
      FulfilledDemandRequest(4000, 2, PopulationDemandRequest(pop, Fruit, 4000)), FulfilledDemandRequest(4000, 0.5,
        PopulationDemandRequest(pop, Coal, 4000))))


    pop.fulfillNeedsUsingAlreadyReceivedProducts()
    pop2.fulfillNeedsUsingAlreadyReceivedProducts()
    assert(pop.needsFulfillment(1).head.needsFulfillment === Map(LifeNeeds -> 1, RegularNeeds -> 1, LuxuryNeeds -> 1))
    assert(pop2.needsFulfillment(1).head.needsFulfillment === Map(LifeNeeds -> 0, RegularNeeds -> 0, LuxuryNeeds -> 0))


    PopulationPromotionDemotion.promoteOrDemote(regionPopulation, random)
    val traders = regionPopulation.pop(Traders, TestCulture)
    assert(traders.populationCount === 996)
    assert(traders.literateCount === 101)

    // promotion from traders
    val capitalists = regionPopulation.pop(Capitalists, TestCulture)
    assert(capitalists.populationCount === 8)
    assert(capitalists.literateCount === 8)

    // movement from capitalists
    val aristocrats = regionPopulation.pop(Aristocrats, TestCulture)
    assert(aristocrats.populationCount === 1)
    assert(aristocrats.literateCount === 1)

    // random movement from traders
    val craftsmen = regionPopulation.pop(Craftsmen, TestCulture)
    assert(craftsmen.populationCount === 5)
    assert(craftsmen.literateCount === 0)
  }

  test("demotion") {
    val random = new Random(0) {
      override def nextInt(n: Int): Int = 0
    }

    val pop = new Population(TestCulture, Traders, 1000, 10000, 100)
    val pop2 = new Population(TestCulture, Farmers, 10, 0, 0)
    val regionPopulation = new RegionPopulation(List(pop, pop2))


    pop2.buyDemandedProducts(List(FulfilledDemandRequest(600, 1, PopulationDemandRequest(pop2, Grain, 600)),
      FulfilledDemandRequest(400, 2, PopulationDemandRequest(pop2, Fruit, 400)), FulfilledDemandRequest(400, 0.5,
        PopulationDemandRequest(pop2, Coal, 400))))


    pop.fulfillNeedsUsingAlreadyReceivedProducts()
    pop2.fulfillNeedsUsingAlreadyReceivedProducts()
    assert(pop.needsFulfillment(1).head.needsFulfillment === Map(LifeNeeds -> 0, RegularNeeds -> 0, LuxuryNeeds -> 0))
    assert(pop2.needsFulfillment(1).head.needsFulfillment === Map(LifeNeeds -> 1, RegularNeeds -> 1, LuxuryNeeds -> 1))


    PopulationPromotionDemotion.promoteOrDemote(regionPopulation, random)
    val traders = regionPopulation.pop(Traders, TestCulture)
    assert(traders.populationCount === 965)
    assert(traders.literateCount === 100)

    val farmers = regionPopulation.pop(Farmers, TestCulture)
    assert(farmers.populationCount === 39)
    assert(farmers.literateCount === 0)

    // random movement from traders and farmers
    val craftsmen = regionPopulation.pop(Craftsmen, TestCulture)
    assert(craftsmen.populationCount === 6)
    assert(craftsmen.literateCount === 0)
  }

}
