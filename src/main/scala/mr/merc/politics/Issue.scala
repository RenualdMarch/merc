package mr.merc.politics

import scalafx.scene.paint.Color
import mr.merc.economics.MapUtil.FloatOperations._
import mr.merc.economics.Population
import mr.merc.economics.Population.{Aristocrats, Bureaucrats, Capitalists, Clergy, Craftsmen, Culture, Farmers, Labourers, LatinHuman, Mages, MagicalAristocrats, Middle, PopulationType, Traders, Upper}
import mr.merc.politics.IssuePosition._

import scala.util.Random

trait IssuePosition extends Product {
  def color:Color
  def name: String = productPrefix
  def populationModifier(pop: Population):Double
  def positionNumber: Int
}

object IssuePosition {
  trait MigrationPosition extends IssuePosition

  trait RegimePosition extends IssuePosition

  trait EconomyPosition extends IssuePosition

  trait ForeignPolicyPosition extends IssuePosition

  trait VotersPolictyPosition extends IssuePosition

  trait SocialPolicyPosition extends IssuePosition
}

class IssuePositionPopularity[T <: IssuePosition](initialPositions:Map[T, Double]) {
  private var positions = initialPositions.scaleToSum(1d)

  def popularity:Map[T, Double] = positions

  def increasePositionPopularity(pop: Population, add: Double, position: T): Unit = {
    positions += position -> (add + position.populationModifier(pop)*positions(position))
    positions = positions.scaleToSum(1d)
  }
}

trait Issue[T <: IssuePosition] {
  type Popularity = IssuePositionPopularity[T]

  val possiblePositions:Vector[T]

  def distanceBetweenPositions(first: T, second: T): Int = {
    math.abs(first.positionNumber - second.positionNumber)
  }

  def name: String
}

object Migration extends Issue[MigrationPosition] {

  case object OpenBorders extends MigrationPosition {
    override def color: Color = Color.Yellow

    override def populationModifier(pop: Population): Double = {
      0.5 + pop.literacy
    }

    override def positionNumber: Int = 0
  }

  case object ClosedBorders extends MigrationPosition {
    override def color: Color = Color.Black

    override def populationModifier(pop: Population): Double = {
      1.5 - pop.literacy
    }

    override def positionNumber: Int = 1
  }

  def popularity(openBorders: Double, closedBorders: Double): Popularity = {
    new IssuePositionPopularity(Map(OpenBorders -> openBorders, ClosedBorders -> closedBorders))
  }

  override def name: String = "migration"

  override val possiblePositions: Vector[MigrationPosition] = Vector(OpenBorders, ClosedBorders)
}

object Regime extends Issue[RegimePosition] {

  case object Absolute extends RegimePosition {
    override def color: Color = Color.Gray

    override def populationModifier(pop: Population): Double = {
      val x = 1 - pop.literacy
      2 * x * x
    }

    override def positionNumber: Int = 0
  }

  case object Constitutional extends RegimePosition {
    override def color: Color = Color.Blue

    override def populationModifier(pop: Population): Double = 0.5 + pop.literacy

    override def positionNumber: Int = 1
  }

  case object Democracy extends RegimePosition {
    override def color: Color = Color.Yellow

    override def populationModifier(pop: Population): Double = 2 * pop.literacy * pop.literacy

    override def positionNumber: Int = 2
  }

  override val possiblePositions: Vector[RegimePosition] = Vector(Absolute, Constitutional, Democracy)

  override def name: String = "regime"

  def popularity(absoulute: Double, constitutional: Double, democracy: Double): Popularity = {
    new IssuePositionPopularity(Map(Absolute -> absoulute, Constitutional -> constitutional, Democracy -> democracy))
  }
}

object Economy extends Issue[EconomyPosition] {

  case object StateEconomy extends EconomyPosition {
    override def color: Color = Color.Red

    override def populationModifier(pop: Population): Double = {
      if (pop.populationType == Bureaucrats) 5.0 else 1.0
    }

    override def positionNumber: Int = 0
  }

  case object Interventionism extends EconomyPosition {
    override def color: Color = Color.Blue

    override def populationModifier(pop: Population): Double = 1.0

    override def positionNumber: Int = 1
  }

  case object FreeMarket extends EconomyPosition {
    override def color: Color = Color.Yellow

    override def populationModifier(pop: Population): Double = if (Set(Traders, Capitalists).contains(pop.populationType)) 5.0 else 1.0

    override def positionNumber: Int = 2
  }

  def popularity(state:Double, interventionism: Double, freeMarket: Double): Popularity = {
    new IssuePositionPopularity(Map(StateEconomy -> state, Interventionism -> interventionism, FreeMarket -> freeMarket))
  }

  override def name: String = "economy"

  override val possiblePositions: Vector[EconomyPosition] = Vector(StateEconomy, Interventionism, FreeMarket)
}

object ForeignPolicy extends Issue[ForeignPolicyPosition] {

  case object Expansionism extends ForeignPolicyPosition {
    override def color: Color = Color.Red

    override def populationModifier(pop: Population): Double =
      if(pop.populationType == Aristocrats) 5.0 else 1.0

    override def positionNumber: Int = 0
  }

  case object Pacifism extends ForeignPolicyPosition {
    override def color: Color = Color.Green

    override def populationModifier(pop: Population): Double =
      if (pop.populationType == Traders) 5.0 else 1.0

    override def positionNumber: Int = 1
  }

  def popularity(expansionism:Double, pacifism: Double): Popularity = {
    new IssuePositionPopularity(Map(Expansionism -> expansionism, Pacifism -> pacifism))
  }

  override def name: String = "foreignPolicy"

  override val possiblePositions: Vector[ForeignPolicyPosition] = Vector(Expansionism, Pacifism)
}

object SocialPolicy extends Issue[SocialPolicyPosition] {

  case object NoSocialSecurity extends SocialPolicyPosition {
    override def color: Color = Color.Gray

    override def populationModifier(pop: Population): Double =
      if (Set(Capitalists, Aristocrats, MagicalAristocrats).contains(pop.populationType)) 5.0 else 1.0

    override def positionNumber: Int = 0
  }
  case object LifeNeedsSocialSecurity extends SocialPolicyPosition {
    override def color: Color = Color.Yellow

    override def populationModifier(pop: Population): Double = 1.0

    override def positionNumber: Int = 1
  }
  case object RegularNeedsSocialSecurity extends SocialPolicyPosition {
    override def color: Color = Color.Red

    override def populationModifier(pop: Population): Double = 1.0

    override def positionNumber: Int = 2
  }

  def popularity(no:Double, lifeNeeds: Double, regularNeeds: Double): Popularity = {
    new IssuePositionPopularity(Map(NoSocialSecurity -> no, LifeNeedsSocialSecurity -> lifeNeeds,
      RegularNeedsSocialSecurity -> regularNeeds))
  }

  override def name: String = "socialPolicy"

  override val possiblePositions: Vector[SocialPolicyPosition] = Vector(NoSocialSecurity, LifeNeedsSocialSecurity, RegularNeedsSocialSecurity)
}

object VotersPolicy extends Issue[VotersPolictyPosition] {

  case object NoVoting extends VotersPolictyPosition {
    override def color: Color = Color.Gray

    override def populationModifier(pop: Population): Double = 2 * (1 - pop.literacy)

    override def positionNumber: Int = 0

  }
  case object PrimaryUpperClass extends VotersPolictyPosition {
    override def color: Color = Color.Purple

    override def populationModifier(pop: Population): Double = {
      if (pop.populationType.populationClass == Upper) 5.0
      else 1.0
    }

    override def positionNumber: Int = 1
  }
  case object PrimaryUpperAndMiddleClass extends VotersPolictyPosition {
    override def color: Color = Color.Blue

    override def populationModifier(pop: Population): Double = {
      if (pop.populationType.populationClass == Upper || pop.populationType.populationClass == Middle) 5.0
      else 1.0
    }

    override def positionNumber: Int = 2
  }
  case object Everyone extends VotersPolictyPosition {
    override def color: Color = Color.Yellow

    override def populationModifier(pop: Population): Double = 5 * pop.literacy

    override def positionNumber: Int = 3

  }
  case object MagesOnly extends VotersPolictyPosition {
    override def color: Color = Color.Orange

    override def populationModifier(pop: Population): Double = {
      if (Set(Mages, MagicalAristocrats).contains(pop.populationType)) 10.0
      else 1 - pop.literacy
    }

    override def positionNumber: Int = 1
  }
  case object ClericsOnly extends VotersPolictyPosition {
    override def color: Color = Color.Green

    override def populationModifier(pop: Population): Double = {
      if (pop.populationType == Clergy) 10.0
      else 1 - pop.literacy
    }

    override def positionNumber: Int = 1
  }

  def popularity(no: Double, upperClass:Double, upperMiddle: Double, everyone: Double, mages: Double, clerics: Double): Popularity = {
    new IssuePositionPopularity(Map(NoVoting -> no, PrimaryUpperClass -> upperClass,
      PrimaryUpperAndMiddleClass -> upperMiddle, Everyone -> everyone, MagesOnly -> mages, ClericsOnly -> clerics))
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
}

object PoliticalViews {
  def initPoliticalViews(populationType: PopulationType, literacy:Double): PoliticalViews = {
    val fakePopulation = new Population(LatinHuman, populationType, 10000, 0, literacy * 10000 toInt, averagePoliticalViews)
    val newViews = averagePoliticalViews
    val shiftStrength = 0.1
    val shiftsCount = 10
    applyShifts(fakePopulation, newViews, shiftStrength, shiftsCount, Migration, x => x.migration)
    applyShifts(fakePopulation, newViews, shiftStrength, shiftsCount, Regime, x => x.regime)
    applyShifts(fakePopulation, newViews, shiftStrength, shiftsCount, ForeignPolicy, x => x.foreignPolicy)
    applyShifts(fakePopulation, newViews, shiftStrength, shiftsCount, Economy, x => x.economy)
    applyShifts(fakePopulation, newViews, shiftStrength, shiftsCount, SocialPolicy, x => x.socialPolicy)
    applyShifts(fakePopulation, newViews, shiftStrength, shiftsCount, VotersPolicy, x => x.votersPolicy)

    newViews
  }

  def averagePoliticalViews:PoliticalViews = {
    PoliticalViews(Migration.popularity(1, 1),
      Regime.popularity(1, 1, 1),
      ForeignPolicy.popularity(1, 1),
      Economy.popularity(1, 1, 1),
      SocialPolicy.popularity(1, 1, 1),
      VotersPolicy.popularity(1 ,1 , 1, 1, 1, 1))
  }

  def applyShifts[T <: IssuePosition](population: Population, views: PoliticalViews, shiftStrength: Double, shiftsCount: Int, issue: Issue[T], extractor:PoliticalViews => IssuePositionPopularity[T]): Unit = {
    randomPopularityShifts(issue).take(shiftsCount).foreach { shift =>
      extractor(views).increasePositionPopularity(population, shiftStrength, shift)
    }
  }

  private def randomPopularityShifts[T <: IssuePosition](issue:Issue[T]):Stream[T] = {
    Stream.continually {
      val i = Random.nextInt(issue.possiblePositions.size)
      issue.possiblePositions(i)
    }
  }

  def sumViews(firstCount: Int, firstViews: PoliticalViews, secondCount: Int, secondViews:PoliticalViews):PoliticalViews = {
    def combine[T](map1:Map[T, Double], map2:Map[T, Double]):Map[T, Double] = {
      (map1 |*| firstCount) |+| (map2 |*| secondCount)
    }

    PoliticalViews(
      new IssuePositionPopularity(combine(firstViews.migration.popularity, secondViews.migration.popularity)),
      new IssuePositionPopularity(combine(firstViews.regime.popularity, secondViews.regime.popularity)),
      new IssuePositionPopularity(combine(firstViews.foreignPolicy.popularity, secondViews.foreignPolicy.popularity)),
      new IssuePositionPopularity(combine(firstViews.economy.popularity, secondViews.economy.popularity)),
      new IssuePositionPopularity(combine(firstViews.socialPolicy.popularity, secondViews.socialPolicy.popularity)),
      new IssuePositionPopularity(combine(firstViews.votersPolicy.popularity, secondViews.votersPolicy.popularity))
    )
  }
}