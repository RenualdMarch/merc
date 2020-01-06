package mr.merc.politics

import mr.merc.economics.Culture
import mr.merc.economics.TaxPolicy._
import mr.merc.economics._
import mr.merc.local.Localization
import mr.merc.map.hex.TerrainHex
import mr.merc.players.Player
import scalafx.scene.paint.Color

class State(val initialName: String, val primeCulture:Culture, startingMoney: Double, val politicalSystem: PoliticalSystem, val color: Color = Color.Black) {
  def name: String = {
    val governmentType = Localization(primeCulture.cultureInfo.stateForm.monarchy)
    s"$initialName $governmentType"
  }

  def rulingParty:Party = politicalSystem.rulingParty

  val taxPolicy: TaxPolicy = {
    val policy = rulingParty.economy
    new TaxPolicy(Map(
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
  budget.spendingPolicyConfig = SpendingPolicyConfig(1d/3, 1d/3, 0, 1d)

  def toPlayer:Player = Player(initialName, color)
}

class Province(val name: String, var owner: State, val regionMarket: RegionMarket,
               val regionPopulation: RegionPopulation, val hexes: Set[FourSeasonsTerrainHex], val capital: FourSeasonsTerrainHex) extends EconomicRegion {

  private var neighbourProvinces:Option[Set[Province]] = None

  var controller: State = owner

  def initNeighbours(set:Set[Province]): Unit = {
    this.neighbourProvinces = Some(set)
  }

  def neighbours:List[Province] = neighbourProvinces.getOrElse(Set()).toList

  override def economicNeighbours: Set[EconomicRegion] = {
    neighbourProvinces.getOrElse(Set()).asInstanceOf[Set[EconomicRegion]]
  }

  val regionWarriors = new RegionWarriors(Nil, economicNeighbours)

  def totalPopulation:Int = regionPopulation.pops.map(_.populationCount).sum

  def culture:Culture = {
    val cultures = regionPopulation.pops.groupBy(_.culture).mapValues(_.map(_.populationCount).sum)
    if (cultures.isEmpty) owner.primeCulture
    else cultures.maxBy(_._2)._1
  }

}