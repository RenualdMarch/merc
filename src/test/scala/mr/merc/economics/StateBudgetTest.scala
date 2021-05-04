package mr.merc.economics

import mr.merc.army.WarriorViewNames
import mr.merc.economics.Culture.LatinHuman
import mr.merc.economics.Population._
import mr.merc.economics.Products.Grain
import mr.merc.economics.SpendingPolicy.{BureaucratsSalary, Pensions, ScholarsSalary}
import mr.merc.economics.TaxPolicy.{CorporateTax, LowSalaryTax}
import mr.merc.politics.{Party, PoliticalViews, State}
import org.scalatest.FunSuite
import scalafx.scene.paint.Color

class StateBudgetTest extends FunSuite {

  test("Spending of budget money doesnt create pop") {

    val culture = new Culture("testCulture", Humans, "testHouse", Color.Red) {


      override val warriorViewNames: WarriorViewNames = LatinHuman.warriorViewNames
      override val cultureInfo: Culture.CultureInfo = LatinHuman.cultureInfo

      private val map:CornerPopulationNeeds = Map(
        Lower -> Map(
          LifeNeeds -> Map(Grain -> 1),
          RegularNeeds -> Map(Grain -> 2),
          LuxuryNeeds -> Map(Grain -> 3)
        ),
        Middle -> Map(
          LifeNeeds -> Map(Grain -> 2),
          RegularNeeds -> Map(Grain -> 3),
          LuxuryNeeds -> Map(Grain -> 4)
        ),
        Upper -> Map(
          LifeNeeds -> Map(Grain -> 3),
          RegularNeeds -> Map(Grain -> 4),
          LuxuryNeeds -> Map(Grain -> 5)
        ))

      override val needs = PopulationNeeds(map, map)
    }

    val traders = new Population(culture, Traders, 1000, 0, 0, PoliticalViews.averagePoliticalViews)
    val bureaucrats = new Population(culture, Bureaucrats, 1000, 0, 0, PoliticalViews.averagePoliticalViews)
    val farmers = new Population(culture, Farmers, 10000, 0, 0, PoliticalViews.averagePoliticalViews)

    val region = new EconomicRegion {
      override def owner: State = new State("testState", culture, 0, Party.absolute, 0)
      owner.taxPolicy.set(TaxPolicy.zeroTaxes.taxPolicyValues)
      owner.budget.refreshTaxPolicy()


      override def economicNeighbours: Set[EconomicRegion] = Set()
      override val regionWarriors: RegionWarriors = null
      override val regionMarket: RegionMarket = new RegionMarket(Map(Grain -> 0.1))
      override val regionPopulation: RegionPopulation = new RegionPopulation(List(traders, bureaucrats, farmers))
    }

    region.regionPopulation.pops.foreach { p =>
      p.newDay(TaxPolicy.zeroTaxes, 1)
    }

    val budget = region.owner.budget
    budget.receiveTaxes(TaxData(LowSalaryTax, 100000, 10000))
    budget.receiveTaxes(TaxData(CorporateTax, 500000, 5000))

    assert(budget.dayReport === BudgetDayReport(budget.taxPolicy.taxPolicyValues, Map(LowSalaryTax -> 10000, CorporateTax -> 5000),
      Map(LowSalaryTax -> 100000, CorporateTax -> 500000), Map()))
    assert(budget.moneyReserve === 15000)

    budget.spendingPolicyConfig = SpendingPolicyConfig(1d/3, 0.5, 1d/6, 1d)

    budget.spendBudgetMoney(List(region), culture)

    assert(region.regionPopulation.pops.count(_.populationType == Scholars) === 0)

    assert(traders.moneyReserves === 0)
    assert(farmers.moneyReserves === 500)
    assert(bureaucrats.moneyReserves === 350)

    budget.endDay()
    assert(budget.yesterdaySpendingPolicy === SpendingPolicy(0, 350, 500))
    assert(budget.history === Vector(BudgetDayReport(budget.taxPolicy.taxPolicyValues, Map(LowSalaryTax -> 10000, CorporateTax -> 5000),
      Map(LowSalaryTax -> 100000, CorporateTax -> 500000), Map(ScholarsSalary -> 0, BureaucratsSalary -> 350, Pensions-> 500))))
  }

  test("Spend budget money") {

    val culture = new Culture("testCulture", Humans, "testHouse", Color.Red) {

      override val warriorViewNames: WarriorViewNames = LatinHuman.warriorViewNames
      override val cultureInfo: Culture.CultureInfo = LatinHuman.cultureInfo

      private val map:CornerPopulationNeeds = Map(
        Lower -> Map(
          LifeNeeds -> Map(Grain -> 1),
          RegularNeeds -> Map(Grain -> 2),
          LuxuryNeeds -> Map(Grain -> 3)
        ),
        Middle -> Map(
          LifeNeeds -> Map(Grain -> 2),
          RegularNeeds -> Map(Grain -> 3),
          LuxuryNeeds -> Map(Grain -> 4)
        ),
        Upper -> Map(
          LifeNeeds -> Map(Grain -> 3),
          RegularNeeds -> Map(Grain -> 4),
          LuxuryNeeds -> Map(Grain -> 5)
        ))

      override val needs = PopulationNeeds(map, map)
    }

    val scholars = new Population(culture, Scholars, 1000, 0, 0, PoliticalViews.averagePoliticalViews)
    val bureaucrats = new Population(culture, Bureaucrats, 1000, 0, 0, PoliticalViews.averagePoliticalViews)
    val farmers = new Population(culture, Farmers, 10000, 0, 0, PoliticalViews.averagePoliticalViews)

    val region = new EconomicRegion {
      override def owner: State = new State("testState", culture, 0, Party.absolute, 0)
      owner.taxPolicy.set(TaxPolicy.zeroTaxes.taxPolicyValues)
      owner.budget.refreshTaxPolicy()

      override def economicNeighbours: Set[EconomicRegion] = Set()
      override val regionWarriors: RegionWarriors = null
      override val regionMarket: RegionMarket = new RegionMarket(Map(Grain -> 0.1))
      override val regionPopulation: RegionPopulation = new RegionPopulation(List(scholars, bureaucrats, farmers))
    }

    val budget = region.owner.budget

    region.regionPopulation.pops.foreach { p =>
      p.newDay(TaxPolicy.zeroTaxes, 1)
    }

    budget.receiveTaxes(TaxData(LowSalaryTax, 100000, 10000))
    budget.receiveTaxes(TaxData(CorporateTax, 500000, 5000))

    assert(budget.dayReport === BudgetDayReport(budget.taxPolicy.taxPolicyValues, Map(LowSalaryTax -> 10000, CorporateTax -> 5000),
      Map(LowSalaryTax -> 100000, CorporateTax -> 500000), Map()))
    assert(budget.moneyReserve === 15000)

    budget.spendingPolicyConfig = SpendingPolicyConfig(1d/3, 0.5, 1d/6, 1d)

    budget.spendBudgetMoney(List(region), culture)

    assert(scholars.moneyReserves === 200)
    assert(farmers.moneyReserves === 500)
    assert(bureaucrats.moneyReserves === 350)

    budget.endDay()
    assert(budget.yesterdaySpendingPolicy === SpendingPolicy(200, 350, 500))
    assert(budget.history === Vector(BudgetDayReport(budget.taxPolicy.taxPolicyValues, Map(LowSalaryTax -> 10000, CorporateTax -> 5000),
      Map(LowSalaryTax -> 100000, CorporateTax -> 500000), Map(ScholarsSalary -> 200, BureaucratsSalary -> 350, Pensions-> 500))))
  }

  test("Not enough money in budget") {

    val culture = new Culture("testCulture", Humans, "testHouse", Color.Red) {

      override val warriorViewNames: WarriorViewNames = LatinHuman.warriorViewNames
      override val cultureInfo: Culture.CultureInfo = LatinHuman.cultureInfo

      private val map:CornerPopulationNeeds = Map(
        Lower -> Map(
          LifeNeeds -> Map(Grain -> 1),
          RegularNeeds -> Map(Grain -> 2),
          LuxuryNeeds -> Map(Grain -> 3)
        ),
        Middle -> Map(
          LifeNeeds -> Map(Grain -> 2),
          RegularNeeds -> Map(Grain -> 3),
          LuxuryNeeds -> Map(Grain -> 4)
        ),
        Upper -> Map(
          LifeNeeds -> Map(Grain -> 3),
          RegularNeeds -> Map(Grain -> 4),
          LuxuryNeeds -> Map(Grain -> 5)
        ))

      override val needs = PopulationNeeds(map, map)
    }

    val scholars = new Population(culture, Scholars, 1000, 0, 0, PoliticalViews.averagePoliticalViews)
    val bureaucrats = new Population(culture, Bureaucrats, 1000, 0, 0, PoliticalViews.averagePoliticalViews)
    val farmers = new Population(culture, Farmers, 10000, 0, 0, PoliticalViews.averagePoliticalViews)

    val region = new EconomicRegion {
      override def owner: State = new State("testState", culture, 0, Party.absolute, 0)
      owner.taxPolicy.set(TaxPolicy.zeroTaxes.taxPolicyValues)
      owner.budget.refreshTaxPolicy()

      override def economicNeighbours: Set[EconomicRegion] = Set()
      override val regionWarriors: RegionWarriors = null
      override val regionMarket: RegionMarket = new RegionMarket(Map(Grain -> 0.1))
      override val regionPopulation: RegionPopulation = new RegionPopulation(List(scholars, bureaucrats, farmers))
    }

    region.regionPopulation.pops.foreach { p =>
      p.newDay(TaxPolicy.zeroTaxes, 1)
    }

    val budget = region.owner.budget
    budget.receiveTaxes(TaxData(LowSalaryTax, 100000, 500))
    budget.receiveTaxes(TaxData(CorporateTax, 500000, 25))

    assert(budget.dayReport === BudgetDayReport(budget.taxPolicy.taxPolicyValues, Map(LowSalaryTax -> 500, CorporateTax -> 25),
      Map(LowSalaryTax -> 100000, CorporateTax -> 500000), Map()))
    assert(budget.moneyReserve === 525)

    budget.spendingPolicyConfig = SpendingPolicyConfig(1d/3, 0.5, 1d/6, 1d)

    budget.spendBudgetMoney(List(region), culture)

    assert(scholars.moneyReserves === 100)
    assert(farmers.moneyReserves === 250)
    assert(bureaucrats.moneyReserves === 175)

    budget.endDay()
    assert(budget.yesterdaySpendingPolicy === SpendingPolicy(100, 175, 250))
    assert(budget.history === Vector(BudgetDayReport(budget.taxPolicy.taxPolicyValues, Map(LowSalaryTax -> 500, CorporateTax -> 25),
      Map(LowSalaryTax -> 100000, CorporateTax -> 500000),
      Map(ScholarsSalary -> 100, BureaucratsSalary -> 175, Pensions-> 250))))
  }
}
