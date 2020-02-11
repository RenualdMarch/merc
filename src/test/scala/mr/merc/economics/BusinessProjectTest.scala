package mr.merc.economics

import mr.merc.army.WarriorViewNames
import scalafx.scene.paint.Color
import mr.merc.economics.Population.{Capitalists, Humans}
import mr.merc.economics.Products.{Coal, Grain, Product, Weapons}
import mr.merc.politics.{Party, PoliticalViews, State}
import org.scalatest.FunSuite

class BusinessProjectTest extends FunSuite {

  val culture = new Culture("test", Humans, "testHouse", Color.White) {
    override val warriorViewNames: WarriorViewNames = null
    override val cultureInfo: Culture.CultureInfo = null
  }

  test("business project flow") {

    val project = new BusinessProject(Map(Grain -> 10d, Weapons -> 20d)) {

      override def returnNotSpentMoneyToInvestor(): Unit = ???

      override def takeMoreMoneyFromInvestor(neededSum: Double): Double = ???

      def executeProjectAim(): Unit = ???
    }

    val prices = Map(Grain -> 1d, Weapons -> 2d)
    assert(project.moneyRequired(prices) === 50)

    project.investMoney(25)
    val grainRequest = BusinessDemandRequest(project, Grain, 5)
    val weaponsRequest = BusinessDemandRequest(project, Weapons, 10)
    val requests = project.demandRequests(prices)
    assert(requests.toSet === Set(grainRequest, weaponsRequest))

    val fulfilledRequsts = List(
      FulfilledDemandRequest(5, 1, grainRequest),
      FulfilledDemandRequest(5, 2, weaponsRequest)
    )
    project.buyDemandedProducts(fulfilledRequsts)
    assert(fulfilledRequsts.map(_.currentSpentMoney).sum === 15)
    assert(project.remainingMoney === 10)
    assert(project.alreadyBoughtProducts === Map(Grain -> 5, Weapons -> 5))
    assert(project.remainingProducts === Map(Grain -> 5, Weapons -> 15))
    assert(project.isComplete === false)

    project.investMoney(50)
    val grainRequest2 = BusinessDemandRequest(project, Grain, 5)
    val weaponsRequest2 = BusinessDemandRequest(project, Weapons, 15)
    assert(project.demandRequests(prices).toSet === Set(grainRequest2, weaponsRequest2))

    val fulfilledRequsts2 = List(
      FulfilledDemandRequest(5, 1, grainRequest2),
      FulfilledDemandRequest(15, 2, weaponsRequest2)
    )

    project.buyDemandedProducts(fulfilledRequsts2)
    assert(fulfilledRequsts2.map(_.currentSpentMoney).sum === 35)
    assert(project.remainingMoney === 25)
    assert(project.remainingProducts === Map(Grain -> 0, Weapons -> 0))
    assert(project.isComplete === true)

    assert(project.takeNotSpentMoney() == 25)
    assert(project.remainingMoney === 0)
    assert(project.isComplete === true)
  }

  test("population expand factory") {
    val pops = List(new Population(culture, Capitalists, 100, 1000, 0, PoliticalViews.averagePoliticalViews),
      new Population(culture, Capitalists, 300, 3000, 0, PoliticalViews.averagePoliticalViews))

    val factory = new IndustrialFactory(null, Weapons, 2, 0, 0, 1, 1)

    val project = new PopulationExpandFactory(factory, pops, Map(Coal -> 100))

    project.takeMoreMoneyFromInvestorIfNeeded(Map(Coal -> 1))
    val demand = project.demandRequests(Map(Coal -> 1))
    project.buyDemandedProducts(List(FulfilledDemandRequest(100, 1, demand.head)))

    assert(pops(0).moneyReserves === 975)
    assert(pops(1).moneyReserves === 2925)

    project.executeProjectAim()
    assert(factory.level === 3)
  }

  test("state build factory") {
    val state = new State("", culture, 1000, new PoliticalSystem(Party.absolute))
    val region = new EconomicRegion {
      override def owner: State = state

      override def economicNeighbours: Set[EconomicRegion] = ???

      override val regionWarriors: RegionWarriors = null
      override val regionMarket: RegionMarket = null
      override val regionPopulation: RegionPopulation = null
    }

    val project = new StateBuildFactory(region, Weapons, state, Map(Coal -> 100))
    project.takeMoreMoneyFromInvestorIfNeeded(Map(Coal -> 1))
    val demand = project.demandRequests(Map(Coal -> 1))
    project.buyDemandedProducts(List(FulfilledDemandRequest(100, 1, demand.head)))
    assert(state.budget.moneyReserve === 900)
    assert(project.isComplete === true)
    project.executeProjectAim()
    assert(region.enterprises.size === 1)
    val factory = region.enterprises.head.asInstanceOf[IndustrialFactory]
    assert(factory.product === Weapons)
    assert(factory.level === 1)
  }
}
