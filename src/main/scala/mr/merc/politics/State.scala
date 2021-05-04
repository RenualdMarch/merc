package mr.merc.politics

import mr.merc.economics.Culture
import mr.merc.economics._
import mr.merc.economics.message.DomesticMailBox
import mr.merc.local.Localization
import mr.merc.players.Player
import mr.merc.politics.IssuePosition.MinMax
import mr.merc.technology.TechnologyLevel
import scalafx.scene.paint.Color

class State(val initialName: String, val primeCulture:Culture, startingMoney: Double, startingRulingParty: Party,
            creationTurn:Int, val technologyLevel: TechnologyLevel = new TechnologyLevel(0),
            val color: Color = Color.Black) {
  def name: String = {
    val governmentType = Localization(primeCulture.cultureInfo.stateForm.monarchy)
    s"$initialName $governmentType"
  }

  val mailBox:DomesticMailBox = new DomesticMailBox()

  val politicalSystem = new PoliticalSystem(startingRulingParty, this, creationTurn)

  def rulingParty:Party = politicalSystem.rulingParty

  final val taxPolicy: TaxPolicy = {
    val policy = rulingParty.economy
    policy.minTaxPolicy
    new TaxPolicy(policy.minTaxPolicy)
  }

  val budget: StateBudget = new StateBudget(startingMoney, taxPolicy)
  budget.spendingPolicyConfig = SpendingPolicyConfig(1d/3, 1d/3, 0, 1d)

  def toPlayer:Player = Player(initialName, color)

  def changeBudgetPolicyAfterRulingPartyChange(): Unit = {
    val policy = politicalSystem.rulingParty.economy
    val currentBudget = budget.taxPolicy.taxPolicyValues
    val policyMap = taxPolicy.taxPolicyValues.keys.map { k =>
      k -> putInBounds(currentBudget(k), policy.minMaxTaxPolicy(k))
    }.toMap
    taxPolicy.set(policyMap)
    budget.refreshTaxPolicy()
  }

  private def putInBounds(value: Double, minMax: MinMax[Double]): Double = {
    if (value < minMax.min) minMax.min
    else if (value > minMax.max) minMax.max
    else value
  }
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