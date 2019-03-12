package mr.merc.politics

import mr.merc.economics.Population.Culture
import mr.merc.economics.TaxPolicy._
import mr.merc.economics._
import mr.merc.local.Localization
import mr.merc.map.hex.TerrainHex
import scalafx.scene.paint.Color

class State(initialName: String, val primeCulture:Culture, startingMoney: Double, val rulingParty: Party, val color: Color = Color.Black) {
  def name: String = {
    val governmentType = Localization(Culture.cultureConfig(primeCulture).stateForm.monarchy)
    s"$initialName $governmentType"
  }

  val taxPolicy: TaxPolicy = {
    val policy = rulingParty.economy
    TaxPolicy(Map(
      LowSalaryTax -> policy.salaryTax.min,
      MiddleSalaryTax -> policy.salaryTax.min,
      UpperSalaryTax -> policy.salaryTax.min,
      TransitTax -> policy.transit.min,
      TariffTax -> policy.tariff.min,
      CorporateTax -> policy.corporateTax.min,
      SalesTax -> policy.salesTax.min
    ))
  }

  val budget: StateBudget = new StateBudget(startingMoney, taxPolicy)
  budget.spendingPolicyConfig = SpendingPolicyConfig(1d/3, 1d/3, 0)
}

case class Province(name: String, var owner: State, regionMarket: RegionMarket,
               regionPopulation: RegionPopulation, hexes: Set[TerrainHex], capital: TerrainHex) extends EconomicRegion {

  private var neighbourProvinces:Option[Set[Province]] = None

  def initNeighbours(set:Set[Province]): Unit = {
    this.neighbourProvinces = Some(set)
  }

  override def economicNeighbours: Set[EconomicRegion] = {
    neighbourProvinces.getOrElse(Set()).asInstanceOf[Set[EconomicRegion]]
  }

  def totalPopulation:Int = regionPopulation.pops.map(_.populationCount).sum
}