package mr.merc.economics

import mr.merc.economics.Population._
import mr.merc.economics.Products.{Cement, Clothes, Fabric, Furniture, Glass, Liquor, Lumber, Paper, Steel, Weapons, Wine, _}

object WorldEconomicConstants {

  object Enterprises {
    val FactoryInputMultiplier = 1
    val FactoryOutputMultiplier = 1
    val FactoryStartingResources = 1000

    val ResourceExtractionEfficiency = 1
    val ChurchRitualEfficiency = 1
    val MagicGuildEfficiency = 1

    val FarmStartingResources = 1000
    val MineStartingResources = 1000
    val ChurchStartingResources = 1000
    val MagicGuildStartingResources = 1000

    val FactoryBuildCost:Map[Products.Product, Double] = Map(Timber -> 10000, Iron -> 10000, Coal -> 10000)
    val FactoryExpandCost:Map[Products.Product, Double] = Map(Timber -> 5000, Iron -> 5000, Coal -> 5000)

    val StateEconomicsInvestmentMultiplier:Double = 4
    val InterventionismInvestmentMultiplier:Double = 2
    val FreeMarketInvestmentMultiplier:Double = 1

    val ProfitPartToWorkers = 0.5
    val ProfitPartToOwners = 0.5
    val NoMaintenancePenalty = 0.3
    val FactoryMaintenancePerLevel: Map[Product, Double] = Map()

    val EfficiencyPerOneFactoryLevel = 1000
  }

  object Population {
    val PromotionRequiredPart = 0.005 // always change pop
    val PromotionOptionalPart = 0.03 // change pop if things go rough or very good
    val MaxHappinessToDemote = 0.2
    val MinHappinessToPromote = 0.7

    val maxPop: Map[PopulationType, Double] = Map(
      Craftsmen -> 0.5,
      Farmers -> 0.5,
      Labourers -> 0.5,
      Bureaucrats -> 0.05,
      Clergy -> 0.05,
      Scholars -> 0.1,
      Traders -> 0.05,
      Mages -> 0.3,
      Capitalists -> 0.02,
      Aristocrats -> 0.02,
      MagicalAristocrats -> 0.01
    )

    val needsQ: Map[PopulationClass, Map[PopulationNeedsType, Double]] = Map(
      Lower -> Map(
        LifeNeeds -> 1d,
        RegularNeeds -> 1d,
        LuxuryNeeds -> 1d
      ),
      Middle -> Map(
        LifeNeeds -> 1d,
        RegularNeeds -> 1d,
        LuxuryNeeds -> 1d
      ),
      Upper -> Map(
        LifeNeeds -> 1d,
        RegularNeeds -> 1d,
        LuxuryNeeds -> 1d
      )
    )

    val HappinessDayCount = 5
    val HappinessLifeNeedsMultiplier = 1
    val HappinessRegularNeedsMultiplier = 1
    val HappinessLuxuryNeedsMultiplier = 1
  }

}

object WorldGenerationConstants {

  val ResourcesRarity:Map[GatheredProduct, Double] = Map(
    Grain -> 1,
    //Fish -> 0.7,
    Fruit -> 0.7,
    Cattle -> 1,
    Tea -> 0.2,
    Coffee -> 0.2,
    Opium -> 0.1,
    Cotton -> 0.7,
    Herbs -> 1,
    Timber -> 0.5,
    Coal -> 0.3,
    Iron -> 0.6,
    PreciousMetal -> 0.1
  )

  val MaxResourcesPerRegion = 4
  val MinResourcesPerRegion = 1

  val FactoriesRarity: Map[Products.IndustryProduct, Double] = Map(
    Lumber -> 1,
    Cement -> 1,
    Fabric -> 1,
    Paper -> 1,
    Glass -> 1,
    Steel -> 1,
    Furniture -> 1,
    Liquor -> 1,
    Clothes -> 1,
    Wine -> 1,
    Weapons -> 1
  )

  val MinFactoriesPerRegion = 2
  val MaxFactoriesPerRegion = 3

  val FactoryStartingMoney = 10000
  val FactoryStartingLevel = 1

  val MinProvinceSize = 1
  val MaxProvinceSize = 5

  val PoorMultiplier = 10000
  val MiddleMultiplier = 1000
  val RichMultiplier = 100

  val PoorLiteracy = 0.01
  val MiddleLiteracy = 0.05
  val RichLiteracy = 0.1

  val PoorMoneyPerPerson = 1
  val MiddleMoneyPerPerson = 5
  val RichMoneyPerPerson = 10

  val StateStartingMoney = 100000
  val StateAvgProvinces = 5
  val StateShuffles = 20

  val TradeDaysBeforeStart = 1
}
