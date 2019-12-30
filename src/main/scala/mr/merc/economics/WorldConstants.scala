package mr.merc.economics

import mr.merc.army.WarriorCompetence.{Militia, Professional}
import mr.merc.economics.Population._
import mr.merc.economics.Products.{Cement, Clothes, Fabric, Furniture, Glass, Liquor, Lumber, Paper, Steel, Weapons, Wine, _}
import MapUtil.FloatOperations._
import mr.merc.army.WarriorCompetence

object WorldConstants {

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

    val BankruptStorage = 0.1
    val BankruptMoney = 0.1
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
    val PoliticalHappinessDisagreementMultiplier = 0.1
    val DifferentCulturePoliticalHappinessPenalty = 0.2

    val ScholarsLiteracyLearningIncreaseMultiplier = 0.1
    val MaxLiteracyEfficiencyMultiplier = 10

    // 1% pop growth per year, to 2% per year max
    val BasePopGrowth:Double = 0.01 / 4
    val GrowthRatePerLifeNeed:Double = 0.005 / 4
    val GrowthRatePerRegularNeed:Double = 0.005 / 4

    val HousePerPopulation = 20000
  }

  object Market {
    val PriceIncrease = 1.03
    val EmptySupplyPriceIncrease = 1.5
    val PriceDecrease = 0.97
    val EmptyDemandPriceDecrease = 0.67

    val LowestPossiblePrice = 0.001
  }

  object Taxes {
    val ZeroBureaucracyTaxCollection = 0.1
    val HalfBureaucracyTaxCollection = 0.8

    def taxCollectionPart(bureaucracyPercentageFromMax:Double):Double = {
      val z = ZeroBureaucracyTaxCollection
      val h = HalfBureaucracyTaxCollection
      val s = math.sqrt(0.5)

      val a = 1 - z - (2*h - 1 - z) / (2 * s - 1)
      val b = (2*h - 1 - z) / (2 * s - 1)
      val c = z
      val x = bureaucracyPercentageFromMax
      a*x+b*math.sqrt(x)+c
    }
  }

  object Army {
    private val supply = Map(Weapons -> 100d, Clothes -> 100d, Grain -> 100d, Cattle -> 100d)
    private val recruitmentCost = Map[Product, Double](Weapons -> 500d, Clothes -> 500d)

    val SoldierSupply:Map[WarriorCompetence, Map[Product, Double]] = Map(
      Professional -> (supply |*| 2),
      Militia -> supply)

    val SoldierRecruitmentCost:Map[WarriorCompetence, Map[Product, Double]] = Map(
      Professional -> (recruitmentCost |*| 4),
      Militia -> recruitmentCost
    )

    def NeedsToHP(d:Double):Double = {
      val result = d + 0.33
      if (result > 1) 1 else result
    }

    val HpLossStep = 0.1
    val HpGainStep = 0.05
  }

  object Diplomacy {
    val DeclareWarRelationshipChange:Int  = -20

    val NeutralRelationshipChange:Int = 0
    val WarRelationshipChange:Int = -50
    val SeparatePeaceRelationshipChange = -100
    val SeparatePeaceDuration = 40


    val TruceDuration = 20
    val TruceRelationshipChange = -100
    val BrokenTruceRelationshipChange = -200
    val BrokenTruceDuration = 60

    val DefaultAllianceDuration = 40
    val AllianceRelationshipChange:Int = 50
    val WereTogetherInAllianceTurns = 20
    val AllianceBetrayalDuration = 20
    val AllianceBetrayalRelationshipsChange = -50
    val AllianceRejectionDuration = 10
    val AllianceRejectionRelationshipChange = -20
    val AllianceHonoredDuration = 20
    val AllianceHonoredRelationshipChange = 30

    val VassalRelationshipChange:Int = 100
    val OverlordRelationshipChange:Int = 100
    val SetVassalFreeDuration = 40
    val SetVassalFreeRelationshipChange = 50
    val VassalRevoltingDuration = 20
    val VassalRevoltingRelationshipChange = -100
    val VassalRejectionDuration = 10
    val VassalRejectionRelationshipChange = -20
    val OverlordshipRejectionDuration = 10
    val OverlordshipRejectionRelationshipChange = -20

    val StrongClaimRelationshipBonus = -20
    val WeakClaimRelationshipBonus = -10

    val SameCultureRelationshipBonus = 20
    val SameRaceRelationshipBonus = 10
    val DifferentRaceRelationshipBonus = -10

    val WarriorPowerMultiplier = 1000
    val GDPPowerMultiplier = 1
  }

  object AI {
    val MaxMessagesPerTurn = 3
    val PowerDifferenceForVassalization = 5
    val MinRelationshipForVassalization = 20
    val EnemyPowerDifferenceForHelpAlly = 2
    val MinRelationshipForHelpAlly = 20
    val RelationshipForHate = -50
    val MinRelationshipToStartAlliance = 10
    val PowerDifferenceForPeace = 4
    val PowerDifferenceForWhitePeace = 2
    val PowerDifferenceToStartWar = 3
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
  val StateAvgProvinces = 2
  val StateShuffles = 20

  val TradeDaysBeforeStart = 1
  val WarriorPerPopulation = 10000

  val WorldMapWidth = 50
  val WorldMapHeight = 50
  val HexesPerProvince = 100
  val LandPercentage = 0.7
  val Provinces = (WorldMapHeight * WorldMapWidth * LandPercentage / HexesPerProvince).toInt

}
