package mr.merc.economics

import mr.merc.army.WarriorCompetence.{Militia, Professional}
import mr.merc.economics.Population._
import mr.merc.economics.Products._
import MapUtil.FloatOperations._
import mr.merc.army.WarriorCompetence
import mr.merc.politics.IssuePosition.RegimePosition
import mr.merc.politics.Regime.{Constitutional, Democracy}
import mr.merc.util.CacheFactoryMap

object WorldConstants {

  object Enterprises {
    val EfficiencyPerOneFactoryLevel = 1000

    val FactoryInputMultiplier = 1
    val FactoryOutputMultiplier = 4
    val FactoryStartingResources = EfficiencyPerOneFactoryLevel * FactoryOutputMultiplier

    val ResourceExtractionEfficiency = 3
    val ChurchRitualEfficiency = 3
    val MagicGuildEfficiency = 3

    val FarmStartingResources = EfficiencyPerOneFactoryLevel * ResourceExtractionEfficiency
    val MineStartingResources = EfficiencyPerOneFactoryLevel * ResourceExtractionEfficiency
    val ChurchStartingResources = EfficiencyPerOneFactoryLevel * ChurchRitualEfficiency
    val MagicGuildStartingResources = EfficiencyPerOneFactoryLevel * MagicGuildEfficiency

    val FactoryBuildCost:Map[Products.Product, Double] = Map(Wood -> 1000, Iron -> 1000, Coal -> 1000)
    val FactoryExpandCost:Map[Products.Product, Double] = Map(Wood -> 1000, Iron -> 1000, Coal -> 1000)

    val StateEconomicsInvestmentMultiplier:Double = 3
    val FreeMarketInvestmentMultiplier:Double = 1

    val ProfitPartToWorkers = 0.5
    val ProfitPartToOwners = 0.5

    val NoBankruptDays = 3
    val BankruptSold = 0.1
    val BankruptProduced = 0.1

    val ResourceGatheringStorageReduction = 0d//0.5
    val FactoriesStorageReduction = 0d//0.5
  }

  object Population {
    val MaxProvinceMigrationPart = 0.1
    val MaxOutsideProvinceMigrationPart = 0.05
    val ConsumptionHappinessToMigrateInsideProvince = 0.3
    val ConsumptionHappinessToMigrateOutsideProvince = 0.2
    val EmptyPopConsumptionHappiness = 0.6
    val BureaucratsPercentageForMaxEff = 0.02

    val LiteracyToAssimilationQ = 0.5
    val BattleVictimsPercentage = 0.1

    val RiggedElectionsQ:Map[RegimePosition, Double] = Map(Constitutional -> 0.3, Democracy -> 0.15)
    val ElectionThreshold = 0.03

    val needsQ: Map[PopulationClass, Map[PopulationNeedsType, Double]] = Map(
      Lower -> Map(
        LifeNeeds -> 0.125,
        RegularNeeds -> 0.25,
        LuxuryNeeds -> 1d
      ),
      Middle -> Map(
        LifeNeeds -> 0.125,
        RegularNeeds -> 0.25,
        LuxuryNeeds -> 1d
      ),
      Upper -> Map(
        LifeNeeds -> 1d,
        RegularNeeds -> 2d,
        LuxuryNeeds -> 4d
      )
    )

    val HappinessLifeNeedsMultiplier = 1
    val HappinessRegularNeedsMultiplier = 1
    val HappinessLuxuryNeedsMultiplier = 1
    val PoliticalHappinessDisagreementMultiplier = 0.1
    val DifferentCulturePoliticalHappinessPenalty = 0.2

    val ScholarsLiteracyLearningIncreaseMultiplier = 0.05
    val MaxLiteracyEfficiencyMultiplier = 10

    // 1% pop growth per year, to 2% per year max
    val BasePopGrowth:Double = 0.01 / 4
    val GrowthRatePerLifeNeed:Double = 0.005 / 4
    val GrowthRatePerRegularNeed:Double = 0.005 / 4

    val RebellionPopulationPart = 0.3

    val HousePerPopulation = 10000

    def isPopRebelling(consumptionHappiness: Double, politicalHappiness: Double): Boolean = {
      if (politicalHappiness > 0.5) false
      else consumptionHappiness < 0.3
    }

    def popRebellingChance(consumptionHappiness: Double, politicalHappiness: Double): Double = {
      if (!isPopRebelling(consumptionHappiness, politicalHappiness)) 0
      else Math.max(0d, (0.3 - consumptionHappiness) / 0.3)
    }
  }

  object Market {
    val EmptySupplyPriceIncrease: Double = 1.2
    val EmptyDemandPriceDecrease: Double = 1 / EmptySupplyPriceIncrease

    def newPrice(currentPrice: Double, supply:Double, demand:Double):Double = {
      val price = ((supply, demand) match {
        case (0, 0) => 1d
        case (0, d) => EmptySupplyPriceIncrease + 0.1 * d / (d + 100)
        case (s, 0) => EmptyDemandPriceDecrease - 0.1 * s / (s + 100)
        case (s, d) =>
          val q = s / d
          if (q >= 1) {
            EmptyDemandPriceDecrease + (1 - EmptyDemandPriceDecrease) / q
          } else {
            EmptySupplyPriceIncrease + (1 - EmptySupplyPriceIncrease) * q
          }
      }) * currentPrice

      if (price < LowestPossiblePrice) LowestPossiblePrice
      else if (price > MaxPossiblePrice) MaxPossiblePrice
      else price
    }

    val LowestPossiblePrice = 0.1
    val MaxPossiblePrice = 100
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
    private val militiaSupply = Map(Iron -> 50d, Clothes -> 50d, Grain -> 50d)
    private val proSupply = Map(Weapons -> 200d, Clothes -> 200d, Grain -> 200d)

    private val militiaRecruitmentCost = Map[Product, Double](Iron -> 50d, Clothes -> 50d)
    private val proRecruitmentCost = Map[Product, Double](Weapons -> 200d, Clothes -> 200d)

    import WorldConstants.Technology.increaseValue

    @transient lazy val SoldierSupply: ((WarriorCompetence, Int)) => Map[Product, Double] = CacheFactoryMap.memo[(WarriorCompetence, Int), Map[Product, Double]] {
      case (wc, level) => wc match {
        case WarriorCompetence.Militia => militiaSupply |*| increaseValue(1d, level)
        case WarriorCompetence.Professional => proSupply |*| increaseValue(1d, level)
        case WarriorCompetence.Ruler => Map()
      }
    }

    val SoldierRecruitmentCost:Map[WarriorCompetence, Map[Product, Double]] = Map(
      Professional -> proRecruitmentCost,
      Militia -> militiaRecruitmentCost
    )

    def NeedsToHP(d:Double):Double = {
      val result = d + 0.5
      if (result > 1) 1 else result
    }

    val HpLossStep = 0.1
    val HpGainStep = 0.05
  }

  object Technology {
    val TechPointsMultiplier = 100d

    val WarriorStrengthIncreasePerLevel = 0.05

    def increaseValue(value: Int, techLevel: Int): Int = {
      (value * (1 + WarriorStrengthIncreasePerLevel * techLevel)).toInt
    }

    def increaseValue(value: Double, techLevel: Double): Double = {
      value * (1 + WarriorStrengthIncreasePerLevel * techLevel)
    }

    def pointsToLevel(level: Int): Double = level * 1000

    def pointsForTurn(literacy: Double, scholarsCount: Int): Double = {
      val multiplier = Math.log10(scholarsCount)
      (1 + literacy) / 2 * multiplier * TechPointsMultiplier
    }
  }

  object Diplomacy {
    val DeclareWarRelationshipChange:Int  = -20

    val ElectionCycle = 5 * 4

    val NeutralRelationshipChange:Int = 0
    val WarRelationshipChange:Int = -50
    val SeparatePeaceRelationshipChange = -100
    val SeparatePeaceDuration = 40
    val SanctionsRelationshipChange = -30

    val TruceDuration = 12
    val TruceRelationshipChange = -10

    val WereInWarDuration = 40
    val WereInWarRelationshipChange = -80

    val BrokenTruceRelationshipChange = -200
    val BrokenTruceDuration = 60

    val DefaultAllianceDuration = 40
    val DefaultFriendshipDuration = 40
    val AllianceRelationshipChange:Int = 50
    val FriendshipRelationshipChange:Int = 40
    val AllianceBetrayalDuration = 20
    val AllianceBetrayalRelationshipsChange = -50
    val FriendshipBetrayalDuration = 20
    val FriendshipBetrayalRelationshipsChange = -40
    val AllianceRejectionDuration = 10
    val FriendshipRejectionDuration = 10
    val AllianceRejectionRelationshipChange = -20
    val FriendshipRejectionRelationshipChange = -10
    val AllianceHonoredDuration = 20
    val AllianceHonoredRelationshipChange = 30

    val VassalizationClaimTurns:Int = 20
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

    val NeighboursWithoutClaimsRelationshipBonus = 30

    val ClaimsOnThemRelationshipChange = -25
    val ClaimsOnUsRelationshipChange = -50

    val VassalizationClaimsOnUsRelationshipChange = -30
    val VassalizationClaimsOnThemRelationshipChange = -15

    val SameReligionRelationshipBonus = 20
    val SamePriorityRelationshipBonus = 10
    val NeighbourReligionRelationshipBonus = -5
    val NeighbourPriorityRelationshipBonus = 0
    val OppositeReligionRelationshipBonus = -30
    val OppositePriorityRelationshipBonus = -10

    val WarriorPowerMultiplier = 1000
    val GDPPowerMultiplier = 1

    val BadBoyTurnRecovery = 1
    val BadBoyToRelationsPenalty = -5
    val SeparatePeaceBadBoy = 3
    val NoCasusBelliWarBadBoy = 3
    val RefuseAllyCallBadBoy = 5
    val AnnexedProvinceWithoutClaimBadBoy = 5
    val AnnexedStateBadBoy = 10
    val CrackedStateBadBoy = 5
    val VassalizedStateBadBoy = 3
    val LiberateCultureBadBoy = 2

    val ClaimCreationBadBoy = 2
    val ClaimDroppingBadBoy = 2
    val LeavingAllianceBadBoy = RefuseAllyCallBadBoy
    val CancellingFriendshipBadBoy = 3

    val RivalOfRivalRelationshipBonus = 30
    val BadBoyToRelationshipConversion = Vector(-5d, -4d, -3d, -2d, -1d)

    val ChanceForInitialWeakClaim = 0.1
    val ChanceForWeakClaim = 0.05
    val WeakClaimTime = 10

    val StalledWarTimeUntilPeace = 8
    val RelationsForBeingFriend = 50
    val RelationsForBeingEnemy = -50

    val RelationsForBeingNeighboursAtPeace = 40

    val DeleteLastTurnEvents = false

    def reputationDescriptionTextKey(badBoy: Double):String = {
      if (badBoy == 0) "diplomacy.reputation.honorable"
      else if (badBoy <= 5) "diplomacy.reputation.respectable"
      else if (badBoy <= 10) "diplomacy.reputation.tarnished"
      else if (badBoy <= 15) "diplomacy.reputation.bad"
      else "diplomacy.reputation.hated"
    }
  }

  object AI {
    val MaxMessagesPerTurn = 3
    val PowerDifferenceForVassalization = 5
    val PowerDifferenceForStoppingBeingAVassal = 2
    val MinRelationshipForVassalization = 20
    val EnemyPowerDifferenceForHelpAlly = 2
    val MinRelationshipForHelpAlly = 20
    val RelationshipForHate = -50
    val MinRelationshipToStartAlliance = 50
    val MinRelationshipForStartFriendship = 0
    val MaxRelationshipToStopFriendship = 0
    val MaxRelationshipToStopAlliance = 0
    val PowerDifferenceForPeace = 4
    val PowerDifferenceForWhitePeace = 2
    val PowerDifferenceToStartWar = 3
    val MaxBudgetSpendingOnWarriors = 0.3
    val TotalArmySumToBeTiredOfWar = 5

    def MinRelationsToStartWar(aggressiveness: Int): Int = {
      aggressiveness - 50
    }
  }

  object Animation {
    val SoldierViewSpriteChangeDuration = 70
    val SoldierAttackSpeed = 300
    val SoldierMovementSpeed = 300
  }
}

object WorldGenerationConstants {

  val ResourcesRarity:Map[GatheredProduct, Double] = Map(
    Grain -> 2,
    //Fish -> 0.7,
    Fruit -> 0.7,
    Cattle -> 1,
    Tea -> 0.4,
    Coffee -> 0.4,
    //Opium -> 0.1
    Cotton -> 1,
    Herbs -> 1,
    Wood -> 1,
    Coal -> 1,
    Iron -> 1,
    PreciousMetal -> 0.2
  )

  val MaxResourcesPerRegion = 10
  val MinResourcesPerRegion = 3

  val FactoriesRarity: Map[Products.IndustryProduct, Double] = Map(
    Paper -> 1,
    Furniture -> 1,
    Medicine -> 1,
    Liquor -> 1,
    Clothes -> 1,
    Wine -> 1,
    Jewelry -> 1,
    Weapons -> 1
  )

  val MinFactoriesPerRegion = 1
  val MaxFactoriesPerRegion = 2

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

  val PopMigrationToNeighbourPercentage = 0.2
  val PopMigrationsToNeighbours = 1

  val LandPercentage = 0.75

  val PoliticalIssueVariation = 0.2


  case class WorldMapCreationConf(width: Int, height: Int, hexesPerProvince: Int) {
    val provinces: Int = (width * height * LandPercentage / hexesPerProvince).toInt
  }
}
