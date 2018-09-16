package mr.merc.economics

import mr.merc.economics.Population._
import mr.merc.economics.Products.{Coal, Fruit, Grain}
import org.scalatest.FunSuite

class PopulationTest extends FunSuite {

  val smallNeeds:Map[PopulationNeedsType, Map[Products.Product, Double]] = Map(
    LifeNeeds -> Map(Grain -> 2.0, Coal -> 1.0),
    RegularNeeds -> Map(Grain -> 3.0, Coal -> 2.0),
    LuxuryNeeds -> Map(Fruit -> 3.0))

  object TestRace extends Race
  object TestCulture extends Culture("test", TestRace) {
    override def needs: PopulationNeeds = Map(Middle -> smallNeeds)
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

}
