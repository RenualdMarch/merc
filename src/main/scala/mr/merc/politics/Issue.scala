package mr.merc.politics

import scalafx.scene.paint.Color
import mr.merc.economics.MapUtil.FloatOperations._
import mr.merc.economics.Population._
import mr.merc.economics.TaxPolicy._
import mr.merc.politics.IssuePosition._
import mr.merc.economics.WorldConstants.Enterprises._

import scala.collection.mutable
import scala.util.Random

trait IssuePosition extends Product {
  def color: Color

  def name: String = productPrefix

  def positionNumber: Int
}

object IssuePosition {

  case class MinMax[T](min: T, max: T)

  trait MigrationPosition extends IssuePosition

  trait RegimePosition extends IssuePosition {
    def unfreerRegime:Option[RegimePosition]
    def freerRegime:Option[RegimePosition]
  }

  trait EconomyPosition extends IssuePosition {
    val tariff: MinMax[Double]
    val corporateTax: MinMax[Double]
    val salaryTax: MinMax[Double]
    val salesTax: MinMax[Double]
    val transit: MinMax[Double]

    val stateCanBuildFactory: Boolean
    val stateCanExpandFactory: Boolean
    val capitalistsCanBuildFactory: Boolean
    val capitalistsCanExpandFactory: Boolean
    val stateCanDestroyFactory: Boolean

    val investmentCostMultiplier: Double

    def minTaxPolicy:Map[Income, Double] = Map(
      CorporateTax -> corporateTax.min,
      LowSalaryTax -> salaryTax.min,
      MiddleSalaryTax -> salaryTax.min,
      UpperSalaryTax -> salaryTax.min,
      SalesTax -> salesTax.min,
      TariffTax -> tariff.min,
      TransitTax -> transit.min
    )

    def maxTaxPolicy:Map[Income, Double] = Map(
      CorporateTax -> corporateTax.max,
      LowSalaryTax -> salaryTax.max,
      MiddleSalaryTax -> salaryTax.max,
      UpperSalaryTax -> salaryTax.max,
      SalesTax -> salesTax.max,
      TariffTax -> tariff.max,
      TransitTax -> transit.max
    )
  }

  trait ForeignPolicyPosition extends IssuePosition

  trait VotersPolictyPosition extends IssuePosition {
    def canVote(populationType: PopulationType, primaryCulture: Boolean): Boolean
  }

  trait SocialPolicyPosition extends IssuePosition

}

class IssuePositionPopularity[T <: IssuePosition](illiterateViews: Map[T, Double], literateViews: Map[T, Double]) {
  private val illit = illiterateViews.scaleToSum(1d)
  private val lit = literateViews.scaleToSum(1d)

  def popularity(literacy: Double): Map[T, Double] = {
    require(literacy <= 1 && literacy >= 0, s"literacy is $literacy")
    (lit |*| literacy) |+| (illit |*| (1 - literacy))
  }

}

trait Issue[T <: IssuePosition] {
  type Popularity = IssuePositionPopularity[T]

  val possiblePositions: Vector[T]

  def distanceBetweenPositions(first: T, second: T): Int = {
    math.abs(first.positionNumber - second.positionNumber)
  }

  def name: String
}

object Migration extends Issue[MigrationPosition] {

  case object OpenBorders extends MigrationPosition {
    override def color: Color = Color.Yellow

    override def positionNumber: Int = 0
  }

  case object ClosedBorders extends MigrationPosition {
    override def color: Color = Color.Black

    override def positionNumber: Int = 1
  }

  def popularity(openBordersStart: Double, closedBordersStart: Double,
                 openBordersEnd: Double, closedBordersEnd: Double): Popularity = {
    new IssuePositionPopularity(Map(OpenBorders -> openBordersStart, ClosedBorders -> closedBordersStart),
      Map(OpenBorders -> openBordersEnd, ClosedBorders -> closedBordersEnd))
  }

  override def name: String = "migration"

  override val possiblePositions: Vector[MigrationPosition] = Vector(OpenBorders, ClosedBorders)
}

object Regime extends Issue[RegimePosition] {

  case object Absolute extends RegimePosition {
    override def color: Color = Color.Gray

    override def positionNumber: Int = 0

    override def unfreerRegime: Option[RegimePosition] = None

    override def freerRegime: Option[RegimePosition] = Some(Constitutional)

  }

  case object Constitutional extends RegimePosition {
    override def color: Color = Color.Blue

    override def positionNumber: Int = 1

    override def unfreerRegime: Option[RegimePosition] = Some(Absolute)

    override def freerRegime: Option[RegimePosition] = Some(Democracy)
  }

  case object Democracy extends RegimePosition {
    override def color: Color = Color.Yellow

    override def positionNumber: Int = 2

    override def unfreerRegime: Option[RegimePosition] = Some(Constitutional)

    override def freerRegime: Option[RegimePosition] = None
  }

  override val possiblePositions: Vector[RegimePosition] = Vector(Absolute, Constitutional, Democracy)

  override def name: String = "regime"

  def popularity(absouluteStart: Double, constitutionalStart: Double, democracyStart: Double,
                 absouluteEnd: Double, constitutionalEnd: Double, democracyEnd: Double): Popularity = {
    new IssuePositionPopularity(
      Map(Absolute -> absouluteStart, Constitutional -> constitutionalStart, Democracy -> democracyStart),
      Map(Absolute -> absouluteEnd, Constitutional -> constitutionalEnd, Democracy -> democracyEnd))
  }
}

object Economy extends Issue[EconomyPosition] {

  case object StateEconomy extends EconomyPosition {
    override def color: Color = Color.Red

    override def positionNumber: Int = 0

    val tariff = MinMax(0.3, 1)
    val corporateTax = MinMax(0.3, 1)
    val salaryTax = MinMax(0.3, 1)
    val salesTax = MinMax(0.3, 1)
    val transit = MinMax(0.1, 0.5)

    val stateCanBuildFactory: Boolean = true
    val stateCanExpandFactory: Boolean = true
    val capitalistsCanBuildFactory: Boolean = false
    val capitalistsCanExpandFactory: Boolean = true
    val stateCanDestroyFactory: Boolean = true

    val investmentCostMultiplier:Double = StateEconomicsInvestmentMultiplier
  }

  case object Interventionism extends EconomyPosition {
    override def color: Color = Color.Blue

    override def positionNumber: Int = 1

    val tariff = MinMax(0.1, 0.4)
    val corporateTax = MinMax(0.1, 0.4)
    val salaryTax = MinMax(0.1, 0.4)
    val salesTax = MinMax(0.1, 0.4)
    val transit = MinMax(0.05, 0.2)

    val stateCanBuildFactory: Boolean = true
    val stateCanExpandFactory: Boolean = false
    val capitalistsCanBuildFactory: Boolean = true
    val capitalistsCanExpandFactory: Boolean = true
    val stateCanDestroyFactory: Boolean = true

    val investmentCostMultiplier:Double = InterventionismInvestmentMultiplier
  }

  case object FreeMarket extends EconomyPosition {
    override def color: Color = Color.Yellow

    override def positionNumber: Int = 2

    val tariff = MinMax(0, 0.2)
    val corporateTax = MinMax(0, 0.2)
    val salaryTax = MinMax(0, 0.2)
    val salesTax = MinMax(0, 0.2)
    val transit = MinMax(0, 0.1)

    val stateCanBuildFactory: Boolean = false
    val stateCanExpandFactory: Boolean = false
    val capitalistsCanBuildFactory: Boolean = true
    val capitalistsCanExpandFactory: Boolean = true
    val stateCanDestroyFactory: Boolean = false

    val investmentCostMultiplier:Double = FreeMarketInvestmentMultiplier
  }

  def popularity(stateStart: Double, interventionismStart: Double, freeMarketStart: Double,
                 stateEnd: Double, interventionismEnd: Double, freeMarketEnd: Double): Popularity = {
    new IssuePositionPopularity(
      Map(StateEconomy -> stateStart, Interventionism -> interventionismStart, FreeMarket -> freeMarketStart),
      Map(StateEconomy -> stateEnd, Interventionism -> interventionismEnd, FreeMarket -> freeMarketEnd))
  }

  override def name: String = "economy"

  override val possiblePositions: Vector[EconomyPosition] = Vector(StateEconomy, Interventionism, FreeMarket)
}

object ForeignPolicy extends Issue[ForeignPolicyPosition] {

  case object Expansionism extends ForeignPolicyPosition {
    override def color: Color = Color.Red

    override def positionNumber: Int = 0
  }

  case object Pacifism extends ForeignPolicyPosition {
    override def color: Color = Color.Green

    override def positionNumber: Int = 1
  }

  def popularity(expansionismStart: Double, pacifismStart: Double,
                 expansionismEnd: Double, pacifismEnd: Double): Popularity = {
    new IssuePositionPopularity(
      Map(Expansionism -> expansionismStart, Pacifism -> pacifismStart),
      Map(Expansionism -> expansionismEnd, Pacifism -> pacifismEnd))
  }

  override def name: String = "foreignPolicy"

  override val possiblePositions: Vector[ForeignPolicyPosition] = Vector(Expansionism, Pacifism)
}

object SocialPolicy extends Issue[SocialPolicyPosition] {

  case object NoSocialSecurity extends SocialPolicyPosition {
    override def color: Color = Color.Gray

    override def positionNumber: Int = 0
  }

  case object LifeNeedsSocialSecurity extends SocialPolicyPosition {
    override def color: Color = Color.Yellow

    override def positionNumber: Int = 1
  }

  case object RegularNeedsSocialSecurity extends SocialPolicyPosition {
    override def color: Color = Color.Red

    override def positionNumber: Int = 2
  }

  def popularity(noStart: Double, lifeNeedsStart: Double, regularNeedsStart: Double,
                 noEnd: Double, lifeNeedsEnd: Double, regularNeedsEnd: Double): Popularity = {
    new IssuePositionPopularity(
      Map(NoSocialSecurity -> noStart, LifeNeedsSocialSecurity -> lifeNeedsStart,
        RegularNeedsSocialSecurity -> regularNeedsStart),
      Map(NoSocialSecurity -> noEnd, LifeNeedsSocialSecurity -> lifeNeedsEnd,
        RegularNeedsSocialSecurity -> regularNeedsEnd))
  }

  override def name: String = "socialPolicy"

  override val possiblePositions: Vector[SocialPolicyPosition] = Vector(NoSocialSecurity, LifeNeedsSocialSecurity, RegularNeedsSocialSecurity)
}

object VotersPolicy extends Issue[VotersPolictyPosition] {

  case object NoVoting extends VotersPolictyPosition {
    override def color: Color = Color.Gray

    override def positionNumber: Int = 0

    override def canVote(populationType: PopulationType, primaryCulture: Boolean): Boolean = false

  }

  case object PrimaryUpperClass extends VotersPolictyPosition {
    override def color: Color = Color.Purple

    override def positionNumber: Int = 1

    override def canVote(populationType: PopulationType, primaryCulture: Boolean): Boolean = primaryCulture && populationType.populationClass == Upper
  }

  case object PrimaryUpperAndMiddleClass extends VotersPolictyPosition {
    override def color: Color = Color.Blue

    override def positionNumber: Int = 2

    override def canVote(populationType: PopulationType, primaryCulture: Boolean): Boolean = primaryCulture && Set(Upper, Middle).contains(populationType.populationClass)
  }

  case object Everyone extends VotersPolictyPosition {
    override def color: Color = Color.Yellow

    override def positionNumber: Int = 3

    override def canVote(populationType: PopulationType, primaryCulture: Boolean): Boolean = true

  }

  case object MagesOnly extends VotersPolictyPosition {
    override def color: Color = Color.Orange

    override def positionNumber: Int = 1

    override def canVote(populationType: PopulationType, primaryCulture: Boolean): Boolean = Mages == populationType
  }

  case object ClericsOnly extends VotersPolictyPosition {
    override def color: Color = Color.Green

    override def positionNumber: Int = 1

    override def canVote(populationType: PopulationType, primaryCulture: Boolean): Boolean = primaryCulture && populationType == Clergy
  }

  def popularity(noStart: Double,
                 upperClassStart: Double,
                 upperMiddleStart: Double,
                 everyoneStart: Double,
                 magesStart: Double,
                 clericsStart: Double,
                 noEnd: Double,
                 upperClassEnd: Double,
                 upperMiddleEnd: Double,
                 everyoneEnd: Double,
                 magesEnd: Double,
                 clericsEnd: Double
                ): Popularity = {
    new IssuePositionPopularity(
      Map(NoVoting -> noStart, PrimaryUpperClass -> upperClassStart,
        PrimaryUpperAndMiddleClass -> upperMiddleStart,
        Everyone -> everyoneStart, MagesOnly -> magesStart, ClericsOnly -> clericsStart),
      Map(NoVoting -> noEnd, PrimaryUpperClass -> upperClassEnd,
        PrimaryUpperAndMiddleClass -> upperMiddleEnd,
        Everyone -> everyoneEnd, MagesOnly -> magesEnd, ClericsOnly -> clericsEnd),
    )
  }

  override def distanceBetweenPositions(first: VotersPolictyPosition, second: VotersPolictyPosition): Int = {
    if (first.positionNumber == second.positionNumber && first != second) {
      1
    } else {
      super.distanceBetweenPositions(first, second)
    }
  }

  override def name: String = "votersPolicy"

  override val possiblePositions: Vector[VotersPolictyPosition] = Vector(NoVoting, PrimaryUpperClass,
    PrimaryUpperAndMiddleClass, Everyone, MagesOnly, ClericsOnly)
}


case class PoliticalViews(migration: Migration.Popularity,
                          regime: Regime.Popularity,
                          foreignPolicy: ForeignPolicy.Popularity,
                          economy: Economy.Popularity,
                          socialPolicy: SocialPolicy.Popularity,
                          votersPolicy: VotersPolicy.Popularity) {

  def currentViews(literacy: Double): CurrentPoliticalViews = {
    CurrentPoliticalViews(
      migration.popularity(literacy),
      regime.popularity(literacy),
      foreignPolicy.popularity(literacy),
      economy.popularity(literacy),
      socialPolicy.popularity(literacy),
      votersPolicy.popularity(literacy))
  }
}

case class CurrentPoliticalViews(migration: Map[MigrationPosition, Double],
                                 regime: Map[RegimePosition, Double],
                                 foreignPolicy: Map[ForeignPolicyPosition, Double],
                                 economy: Map[EconomyPosition, Double],
                                 socialPolicy: Map[SocialPolicyPosition, Double],
                                 votersPolicy: Map[VotersPolictyPosition, Double]) {
  def pointsOfView:Map[PoliticalPosition, Double] = {
    val positions = for {
      m <- Migration.possiblePositions
      r <- Regime.possiblePositions
      fp <- ForeignPolicy.possiblePositions
      e <- Economy.possiblePositions
      sp <- SocialPolicy.possiblePositions
      vp <- VotersPolicy.possiblePositions
    } yield {
      PoliticalPosition(m, r, fp, e, sp, vp) ->
        (migration(m) * regime(r) * foreignPolicy(fp) *
        economy(e) * socialPolicy(sp) * votersPolicy(vp))
    }
    positions.filter(_._2 > 0).toMap
  }
}

case class PoliticalPosition(migration: MigrationPosition,
                             regime: RegimePosition,
                             foreignPolicy: ForeignPolicyPosition,
                             economy: EconomyPosition,
                             socialPolicy: SocialPolicyPosition,
                             votersPolicy: VotersPolictyPosition) {

  private val memo = mutable.Map[PoliticalPosition, Int]()

  def diffWithPosition(o: PoliticalPosition): Int = {
    def f(other: PoliticalPosition): Int = {
      Migration.distanceBetweenPositions(other.migration, migration) +
        Regime.distanceBetweenPositions(other.regime, regime) +
        ForeignPolicy.distanceBetweenPositions(other.foreignPolicy, foreignPolicy) +
        Economy.distanceBetweenPositions(other.economy, economy) +
        SocialPolicy.distanceBetweenPositions(other.socialPolicy, socialPolicy) +
        VotersPolicy.distanceBetweenPositions(other.votersPolicy, votersPolicy)
    }

    memo.get(o) match {
      case Some(r) => r
      case None =>
        val v = f(o)
        memo.put(o, v)
        v
    }
  }

}


object PoliticalViews {
  def initPoliticalViews(populationType: PopulationType, random: Random = Random): PoliticalViews = {
    implicit class variation(d:Double) {
      def r: Double = d + random.nextDouble() * 1
    }
    def migration = Migration.popularity(0.r, 1.r, 1.r, 0.r)
    def regime = Regime.popularity(1.r, 0.5.r, 0.r, 0.r, 0.5.r, 1.r)
    def foreignPolicy = ForeignPolicy.popularity(0.7.r, 0.3.r, 0.3.r, 0.7.r)
    def aristocraticPolicy = ForeignPolicy.popularity(1.r, 0.r, 0.8.r, 0.2.r)
    def poorEconomy = Economy.popularity(1.r, 0.5.r, 0.5.r, 0.5.r, 0.5.r, 0.5.r)
    def middleEconomy = Economy.popularity(0.5.r, 1.r, 0.5.r, 0.5.r, 0.5.r, 0.5.r)
    def aristorcraticEconomy = Economy.popularity(1.r, 0.5.r, 0.r, 0.r, 0.5.r, 1.r)
    def capitalistEconomy = Economy.popularity(0.r, 0.5.r, 0.5.r, 0.r, 0.r, 1.r)
    def poorSocial = SocialPolicy.popularity(0.2.r, 0.3.r, 0.4.r, 0.2.r, 0.5.r, 0.8.r)
    def mediumSocial = SocialPolicy.popularity(0.5.r, 0.3.r, 0.2.r, 1.r, 0.5.r, 0.r)
    def upperSocial = SocialPolicy.popularity(1.r, 0.r, 0.r, 1.r, 0.2.r, 0.r)
    def poorVoters = VotersPolicy.popularity(1.r, 0.5.r, 0.3.r, 0.r, 0.2.r, 0.2.r,
      0.r,0.3.r,0.4.r,1.r,0.1.r,0.1.r)
    def middleVoters = VotersPolicy.popularity(1.r, 0.2.r, 0.5.r, 0.r, 0.2.r, 0.2.r,
      0.r,0.4.r,1.r,0.5.r,0.1.r,0.1.r)
    def upperVoters = VotersPolicy.popularity(1.r, 0.7.r, 0.3.r, 0.r, 0.2.r, 0.2.r,
      0.r,0.8.r,0.6.r,0.2.r,0.1.r,0.1.r)

    populationType match {
      case Craftsmen =>
        PoliticalViews(migration, regime,foreignPolicy, poorEconomy, poorSocial, poorVoters)
      case Farmers =>
        PoliticalViews(migration, regime, foreignPolicy, poorEconomy, poorSocial, poorVoters)
      case Labourers =>
        PoliticalViews(migration, regime, foreignPolicy, poorEconomy, poorSocial, poorVoters)
      case Bureaucrats =>
        PoliticalViews(migration, regime, foreignPolicy,
          Economy.popularity(1.r, 0.5.r, 0.r, 0.5.r, 0.5.r, 0.r),
          mediumSocial, middleVoters)
      case Scholars =>
        PoliticalViews(migration, regime, foreignPolicy, middleEconomy, mediumSocial, middleVoters)
      case Clergy =>
        PoliticalViews(migration, regime, foreignPolicy, middleEconomy, mediumSocial,
          VotersPolicy.popularity(1.r, 0.5.r, 0.3.r, 0.r, 0.r, 1.r,
            1.r, 0.5.r, 0.3.r, 0.r, 0.r, 1.r))
      case Mages =>
        PoliticalViews(migration, regime, foreignPolicy, middleEconomy, mediumSocial,
          VotersPolicy.popularity(1.r, 0.5.r, 0.3.r, 0.r, 1.r, 0.r,
            1.r, 0.5.r, 0.3.r, 0.r, 1.r, 0.r))
      case Traders =>
        PoliticalViews(migration, regime, foreignPolicy,
          Economy.popularity(0.r, 1.r, 0.5.r, 0.r, 0.5.r, 1.r),
          mediumSocial, middleVoters)
      case Capitalists =>
        PoliticalViews(migration, regime, aristocraticPolicy, capitalistEconomy, upperSocial, upperVoters)
      case Aristocrats =>
        PoliticalViews(migration, regime, aristocraticPolicy, aristorcraticEconomy, upperSocial, upperVoters)
    }
  }

  def averagePoliticalViews: PoliticalViews = {
    PoliticalViews(Migration.popularity(1, 1, 1, 1),
      Regime.popularity(1, 1, 1, 1, 1, 1),
      ForeignPolicy.popularity(1, 1, 1, 1),
      Economy.popularity(1, 1, 1, 1, 1, 1),
      SocialPolicy.popularity(1, 1, 1, 1, 1, 1),
      VotersPolicy.popularity(1, 1, 1, 1, 1, 1,1,1,1,1,1,1))
  }

}