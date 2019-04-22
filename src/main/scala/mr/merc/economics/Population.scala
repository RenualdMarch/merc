package mr.merc.economics

import com.typesafe.config.ConfigFactory
import mr.merc.economics.Population._
import mr.merc.economics.Products.{Medicine, _}
import mr.merc.economics.MapUtil.FloatOperations._
import mr.merc.economics.TaxPolicy.{LowSalaryTax, MiddleSalaryTax, UpperSalaryTax}
import mr.merc.map.objects.{House, HumanCityHouse, HumanCottage, HumanVillageHouse}
import mr.merc.politics.PoliticalViews
import scalafx.scene.paint.Color
import pureconfig.loadConfigOrThrow
import pureconfig.generic.auto._
import WorldEconomicConstants.Population._

import scala.util.Random

class Population(val culture: Culture, val populationType: PopulationType, private var count: Double,
                 startingMoney: Double, private val startingliterateCount: Int, var politicalViews: PoliticalViews) {
  require(needs.nonEmpty, s"Needs for culture $culture for type $populationType are empty!")

  private var literatePeople = startingliterateCount

  def literateCount: Int = literatePeople

  private val salaryRecordsMaxSize = 30

  private var tax: Double = 0d
  private var currentPopulationDayRecord: PopulationDayRecord = PopulationDayRecord(populationCount, 0, 0, 0, 0,
    new ProductFulfillmentRecord(Map(), needs, Map()))

  def currentDayRecord: PopulationDayRecord = currentPopulationDayRecord

  def populationCount: Int = count.toInt

  def needs: Map[PopulationNeedsType, Map[Products.Product, Double]] = culture.needs(populationType.populationClass).
    map { case (nt, m) => nt -> m.mapValues(_ * count * efficiency) }

  def needsFulfillment(i: Int):Vector[ProductFulfillmentRecord] = populationDayRecords.takeRight(i).map(_.productFulfillment)

  // last 5 turns
  def happiness: Double = {
    val n = needsFulfillment(HappinessDayCount)
    val sum = HappinessLifeNeedsMultiplier + HappinessRegularNeedsMultiplier + HappinessLuxuryNeedsMultiplier

    if (n.isEmpty) {
      1
    } else {
      n.map { f =>
        val needs = f.needsFulfillment(LifeNeeds) * HappinessLifeNeedsMultiplier +
          f.needsFulfillment(RegularNeeds) * HappinessRegularNeedsMultiplier +
          f.needsFulfillment(LuxuryNeeds) * HappinessLuxuryNeedsMultiplier
        needs / sum
      }.sum / n.size
    }
  }

  def extractLiterateMovers(count: Int): PopulationMovers = {
    val change = if (count < literatePeople) count else literatePeople
    literatePeople -= change
    this.count -= change
    PopulationMovers(change, 0)
  }

  def extractIlliterateMovers(count: Int): PopulationMovers = {
    val change = if (count < populationCount - literatePeople) count else populationCount - literatePeople
    this.count -= change
    PopulationMovers(0, change)
  }

  def extractLiterateThenIlliterate(count: Int): PopulationMovers = {
    val lit = extractLiterateMovers(count)
    if (lit.literateCount < count) {
      val rem = count - lit.literateCount
      extractIlliterateMovers(rem) + lit
    } else {
      lit
    }
  }

  def extractIlliterateThenLiterate(count: Int): PopulationMovers = {
    val illit = extractIlliterateMovers(count)
    if (illit.illiterateCount < count) {
      val rem = count - illit.illiterateCount
      extractLiterateMovers(rem) + illit
    } else {
      illit
    }
  }

  def extractRandomMovers(count: Int): PopulationMovers = {
    val lit = (count * literacy) toInt
    val illit = count - lit
    extractLiterateThenIlliterate(lit) + extractIlliterateThenLiterate(illit)
  }

  def applyMovers(movers: PopulationMovers): Unit = {
    this.count += movers.totalCount
    this.literatePeople += movers.literateCount
  }

  private var populationDayRecords = Vector[PopulationDayRecord]()

  def salary: Vector[PopulationDayRecord] = populationDayRecords

  private var currentPrices: Map[Product, Double] = Map()

  private var currentMoney = startingMoney

  def moneyReserves: Double = currentMoney

  def calculateDemands(prices: Map[Product, Double]): Map[Product, Double] = {
    val lifeNeeds = buyAsMuchAsPossible(moneyReserves, prices, needs(LifeNeeds))
    val regularNeeds = buyAsMuchAsPossible(moneyReserves - moneyCost(prices, lifeNeeds), prices, needs(RegularNeeds))
    val luxNeeds = buyAsMuchAsPossible(moneyReserves - moneyCost(prices, lifeNeeds) - moneyCost(prices, regularNeeds), prices, needs(LuxuryNeeds))
    lifeNeeds |+| regularNeeds |+| luxNeeds
  }

  private def buyAsMuchAsPossible(money: Double, prices: Map[Product, Double], demands: Map[Product, Double]): Map[Product, Double] = {
    val demandsList = demands.toList.flatMap { case (product, productDemand) =>
      prices.get(product).map(price => DemandInfo(product, productDemand, price))
    }.sortBy(_.price)

    demandsList.foldLeft(Map[Product, Double]()) { case (map, DemandInfo(product, productDemand, price)) =>
      val alreadySpent = moneyCost(prices, map)
      val remained = money - alreadySpent
      if (remained <= 0) map
      else {
        val possibleToBuy = remained / price
        val buy = scala.math.min(possibleToBuy, productDemand)
        map + (product -> buy)
      }
    }
  }

  private def moneyCost(prices: Map[Product, Double], bought: Map[Product, Double]): Double = {
    bought.foldLeft(0d) { case (sum, (product, boughtProduct)) =>
      sum + boughtProduct * prices(product)
    }
  }

  def efficiency: Double = 1 + literacy * literacy * (MaxLiteracyEfficiencyMultiplier - 1)

  def totalPopEfficiency: Double = count * efficiency

  def literacy: Double = if (populationCount == 0) 0 else literatePeople.toDouble / populationCount

  private val taxPolicy = this.populationType.populationClass match {
    case Lower => LowSalaryTax
    case Middle => MiddleSalaryTax
    case Upper => UpperSalaryTax
  }

  def newDay(stateTaxPolicy: TaxPolicy, bureaucratsPercentage: Double): Unit = {
    this.tax = stateTaxPolicy.tax(taxPolicy, bureaucratsPercentage)
    this.currentPopulationDayRecord = PopulationDayRecord(populationCount, 0, 0, 0, 0,
      new ProductFulfillmentRecord(Map(), needs, Map()))
  }

  def endOfDay(): Unit = {
    populationDayRecords :+= currentPopulationDayRecord

    if (populationDayRecords.size > salaryRecordsMaxSize) {
      populationDayRecords = populationDayRecords.takeRight(salaryRecordsMaxSize)
    }
  }

  def investMoney(neededSum: Double): Double = {
    if (this.moneyReserves > neededSum) {
      this.currentMoney -= neededSum
      this.currentPopulationDayRecord = currentPopulationDayRecord.copy(
        investments = currentPopulationDayRecord.investments + neededSum)
      neededSum
    } else {
      val existingSum = this.moneyReserves
      this.currentMoney = 0
      this.currentPopulationDayRecord = currentPopulationDayRecord.copy(
        investments = currentPopulationDayRecord.investments + existingSum)
      existingSum
    }
  }

  private case class DemandInfo(product: Product, count: Double, price: Double)

  private var alreadyReceivedProducts: List[FulfilledDemandRequest] = Nil

  def buyDemandedProducts(requests: List[FulfilledDemandRequest]): Double = {
    assert(requests.forall(r => r.request.asInstanceOf[PopulationDemandRequest].pop == this))

    currentPrices = requests.map { f =>
      f.request.product -> f.price
    }.toMap

    alreadyReceivedProducts ++= requests

    val spentMoney = requests.foldLeft(0d) { case (sum, request) =>
      sum + request.bought * request.price
    }

    currentMoney = currentMoney - spentMoney
    spentMoney
  }

  def fulfillNeedsUsingAlreadyReceivedProducts(): Unit = {
    val receivedProductsMap = alreadyReceivedProducts.map { r =>
      Map(r.request.product -> r.bought)
    }.fold(Map())(_ |+| _)

    val productFulfillment = new ProductFulfillmentRecord(currentPrices, needs, receivedProductsMap)
    this.currentPopulationDayRecord = currentPopulationDayRecord.copy(productFulfillment = productFulfillment)
  }

  // TODO add info about salary sources
  def receiveSalary(salary: Double, payTax: Boolean = true): Unit = {
    val taxPart = if (payTax) tax * salary else 0

    currentMoney = salary + currentMoney - taxPart
    val r = this.currentPopulationDayRecord
    this.currentPopulationDayRecord = this.currentPopulationDayRecord.copy(populationCount, r.receivedMoney + salary, currentMoney, r.taxes + taxPart)
  }

  def payTaxes(): TaxData = TaxData(taxPolicy, this.currentPopulationDayRecord.totalMoney, this.currentPopulationDayRecord.taxes)


  override def toString: String = s"$culture $populationType"
}

object Population {
  val MaxLiteracyEfficiencyMultiplier = 10

  sealed abstract class PopulationNeedsType(val needImportance: Int) extends scala.Product with Serializable with Ordered[PopulationNeedsType] {
    override def compare(that: PopulationNeedsType): Int = {
      def toInt(pc: PopulationNeedsType): Int = {
        pc match {
          case LifeNeeds => 1
          case RegularNeeds => 2
          case LuxuryNeeds => 3
        }
      }

      toInt(this) - toInt(that)
    }

    def name: String = this.productPrefix.toLowerCase
  }

  case object LifeNeeds extends PopulationNeedsType(5)

  case object RegularNeeds extends PopulationNeedsType(3)

  case object LuxuryNeeds extends PopulationNeedsType(1)

  sealed trait PopulationClass extends scala.Product with Serializable with Ordered[PopulationClass] {
    override def compare(that: PopulationClass): Int = {
      def toInt(pc: PopulationClass): Int = {
        pc match {
          case Lower => 1
          case Middle => 2
          case Upper => 3
        }
      }

      toInt(this) - toInt(that)
    }

    def name: String = this.productPrefix.toLowerCase
  }

  case object Lower extends PopulationClass

  case object Middle extends PopulationClass

  case object Upper extends PopulationClass

  def populationClasses: List[PopulationClass] = List(Lower, Middle, Upper)

  sealed abstract class PopulationType(val populationClass: PopulationClass) extends scala.Product with Serializable with Ordered[PopulationType] {
    override def compare(that: PopulationType): Int = {
      this.populationClass.compare(that.populationClass)
    }

    def name: String = this.productPrefix.toLowerCase
  }

  //// craftsmen work in factories
  case object Craftsmen extends PopulationType(Lower)

  // farmers gather resources that are plants:
  case object Farmers extends PopulationType(Lower)

  // labourers gather resources in mines
  case object Labourers extends PopulationType(Lower)

  // Bureaucrats work with papers, they are administrators
  case object Bureaucrats extends PopulationType(Middle)

  // scholar teach people literacy and can invent
  case object Scholars extends PopulationType(Middle)

  // priests produce rituals
  case object Clergy extends PopulationType(Middle)

  // mages produce medicine and amulets
  case object Mages extends PopulationType(Middle)

  // traders take percent from market transactions in their region
  case object Traders extends PopulationType(Middle)

  // capitalists own factories and receive profits from them
  case object Capitalists extends PopulationType(Upper)

  // aristocrats own land and receive from resource gathering.
  // They are also best soldiers
  case object Aristocrats extends PopulationType(Upper)

  // same as usual aristocrats
  case object MagicalAristocrats extends PopulationType(Upper)

  val PopulationPromotionDemotion: Map[PopulationType, List[PopulationType]] = Map(
    Craftsmen -> List(Farmers, Labourers, Bureaucrats, Traders, Clergy),
    Farmers -> List(Craftsmen, Labourers, Bureaucrats, Traders, Clergy),
    Labourers -> List(Craftsmen, Farmers, Bureaucrats, Traders, Clergy),
    Bureaucrats -> List(Farmers, Labourers, Craftsmen, Scholars, Traders, Clergy, Capitalists, Aristocrats),
    Scholars -> List(Bureaucrats, Traders, Capitalists, Aristocrats, Clergy),
    Clergy -> List(Craftsmen, Farmers, Labourers, Bureaucrats, Scholars, Traders, Capitalists, Aristocrats),
    Mages -> List(MagicalAristocrats),
    Traders -> List(Craftsmen, Farmers, Labourers, Bureaucrats, Scholars, Clergy, Capitalists, Aristocrats),
    Capitalists -> List(Aristocrats, Traders, Clergy, Scholars, Bureaucrats),
    Aristocrats -> List(Capitalists, Traders, Clergy, Scholars, Bureaucrats),
    MagicalAristocrats -> List(Mages)
  )

  def populationTypes: List[PopulationType] =
    List(Craftsmen, Farmers, Labourers, Bureaucrats, Clergy,
      Scholars, Traders, Mages, Capitalists, Aristocrats, MagicalAristocrats)

  def populationTypesByClass: Map[PopulationClass, List[PopulationType]] =
    populationTypes.groupBy(_.populationClass)

  type ProductBracket = Map[Products.Product, Double]

  type PopulationNeeds = Map[PopulationClass, Map[PopulationNeedsType, ProductBracket]]

  private def scaleNeeds(needsToScale: PopulationNeeds): PopulationNeeds = {
    needsToScale.transform { case (popClass, classNeedsMap) =>
      classNeedsMap.transform { case (popNeedsType, needsMap) =>
        val q = needsQ(popClass)(popNeedsType)
        needsMap.scaleToSum(q)
      }
    }
  }

  private def defaultHumanNeeds(culture: Culture): PopulationNeeds = Map(
    Lower -> Map(
      LifeNeeds -> Map(Grain -> 3, Fruit -> 1, Cattle -> 1),
      RegularNeeds -> Map(Tea -> 1, Clothes -> 1, Liquor -> 1, Furniture -> 1, Coal -> 1,
        Lumber -> 1, Ritual(culture) -> 1),
      LuxuryNeeds -> Map(Magic -> 1, Paper -> 1, Coffee -> 1, Weapons -> 1, Wine -> 1, Medicine -> 1)
    ),
    Middle -> Map(
      LifeNeeds -> Map(Grain -> 2, Fruit -> 1, Cattle -> 1),
      RegularNeeds -> Map(Tea -> 1, Clothes -> 1, Liquor -> 1, Furniture -> 1, Coal -> 1, Ritual(culture) -> 1,
        Glass -> 1, Wine -> 1, Magic -> 1, Medicine -> 1, Paper -> 1, Weapons -> 1, Cement -> 1),
      LuxuryNeeds -> Map(Wine -> 1, Magic -> 1, Medicine -> 1,
        Furniture -> 1, Opium -> 1, Paper -> 1, Jewelry -> 1)
    ),
    Upper -> Map(
      LifeNeeds -> Map(Grain -> 1, Fruit -> 1, Cattle -> 2),
      RegularNeeds -> Map(Tea -> 5, Clothes -> 5, Liquor -> 5, Furniture -> 5, Coal -> 10, Cement -> 5,
        Glass -> 5, Wine -> 10, Magic -> 10, Medicine -> 5, Paper -> 10, Jewelry -> 5, Weapons -> 5, Opium -> 5, Ritual(culture) -> 1),
      LuxuryNeeds -> Map(Furniture -> 5, Coal -> 5, Paper -> 5, Magic -> 5,
        Medicine -> 5, Weapons -> 5, Cement -> 5, Opium -> 5, Ritual(culture) -> 2, Jewelry -> 5)
    )
  )

  // removed sealed for test purposes only
  abstract class Race extends scala.Product with Serializable {
    def name: String = productPrefix.toLowerCase
  }

  case object Humans extends Race

  //
  // DISABLED RACES
  //
  case object Elves extends Race

  case object Dwarfs extends Race

  case object Orcs extends Race

  case object Saurians extends Race

  case object Drakes extends Race

  case object Undead extends Race

  case object Demons extends Race

  object Culture {

    case class StateForm(monarchy: String, democracy: String)

    case class CultureInfo(stateForm: StateForm, cities: List[String], states: List[String])

    // do not remove
    import mr.merc.util.MercUtils.ConfigConvertProtocol.camelCaseHint

    private val config = ConfigFactory.load("conf/cultures.conf")
    val cultureConfig: Map[Culture, CultureInfo] = Population.cultures.map { c =>
      c -> loadConfigOrThrow[CultureInfo](config.getConfig(c.name))
    } toMap
  }

  // removed sealed for test purposes only
  abstract class Culture(val name: String, val race: Race, val houseStyle: House, val color: Color) {
    def needs: PopulationNeeds = scaleNeeds(defaultHumanNeeds(this))

    def cultureNameKey: String = "culture." + name
  }

  case object LatinHuman extends Culture("latin", Humans, HumanCityHouse, Color.Red)

  case object KnightHuman extends Culture("french", Humans, HumanVillageHouse, Color.Blue)

  case object DarkHuman extends Culture("german", Humans, HumanCottage, Color.Black)

  //
  // DISABLED CULTURES
  //
  /*case object HighElf extends Culture("state.federation", Elves)
  case object DarkElf extends Culture("state.confederation", Elves)
  case object BarbarianOrc extends Culture("state.horde", Orcs)
  case object RockDwarf extends Culture("state.clan", Dwarfs)
  case object GreenSaurian extends Culture("state.syndicate", Saurians)
  case object OldDrakes extends Culture("state.dominion", Drakes)
  case object Forsaken extends Culture("state.collective", Undead)
  case object RedDemons extends Culture("state.legion", Demons)
*/
  // good words: alliance, protectorate, tribe, army

  def cultures: List[Culture] = List(LatinHuman, KnightHuman, DarkHuman) //, HighElf, DarkElf, BarbarianOrc, RockDwarf, GreenSaurian, OldDrakes, Forsaken, RedDemons)

  class ProductFulfillmentRecord(prices: Map[Products.Product, Double], needs: Map[PopulationNeedsType, Map[Products.Product, Double]], products: Map[Product, Double]) {

    val needsFulfillmentInfo: Map[PopulationNeedsType, List[ProductFulfillment]] = {

      val lifeNeeds = needs(LifeNeeds)
      val afterLifeNeeds = products |-| lifeNeeds
      val lifeNeedsFulfillment = calculateProductFulfillment(lifeNeeds, afterLifeNeeds)
      val beforeRegularNeeds = removeZeroesAndNegative(afterLifeNeeds)
      val regularNeeds = needs(RegularNeeds)
      val afterRegularNeeds = beforeRegularNeeds |-| regularNeeds
      val regularNeedsFulfillment = calculateProductFulfillment(regularNeeds, afterRegularNeeds)
      val beforeLuxNeeds = removeZeroesAndNegative(afterRegularNeeds)
      val luxuryNeeds = needs(LuxuryNeeds)
      val afterLuxNeeds = beforeLuxNeeds |-| luxuryNeeds
      val luxuryNeedsFulfillment = calculateProductFulfillment(luxuryNeeds, afterLuxNeeds)

      Map(LifeNeeds -> lifeNeedsFulfillment,
        RegularNeeds -> regularNeedsFulfillment,
        LuxuryNeeds -> luxuryNeedsFulfillment)
    }

    val needsFulfillment: Map[PopulationNeedsType, Double] = needsFulfillmentInfo.transform {
      case (_, list) =>
        val totalDemanded = list.map(_.demanded).sum
        val totalReceived = list.map(_.received).sum
        if (totalDemanded == 0) {
          1
        } else {
          totalReceived / totalDemanded
        }
    }

    private def calculateProductFulfillment(needs: Map[Product, Double], afterFulfillment: Map[Product, Double]): List[ProductFulfillment] = {
      needs.toList.map { case (product, productNeed) =>
        val remainedProduct = afterFulfillment.getOrElse(product, -productNeed)
        if (remainedProduct > 0)
          ProductFulfillment(product, productNeed, productNeed, prices.get(product))
        else {
          val received = productNeed + remainedProduct
          ProductFulfillment(product, productNeed, received, prices.get(product))
        }
      }
    }

    private def removeZeroesAndNegative(map: Map[Product, Double]): Map[Product, Double] = map.filter(_._2 > 0)

  }

  case class ProductFulfillment(product: Product, demanded: Double, received: Double, price: Option[Double])

  // TODO add money source
  case class PopulationDayRecord(populationCount: Int, receivedMoney: Double, totalMoney: Double, taxes: Double,
                                 investments: Double, productFulfillment: ProductFulfillmentRecord)

}

case class PopulationMovers(literateCount: Int, illiterateCount: Int) {
  def +(movers: PopulationMovers): PopulationMovers = {
    PopulationMovers(this.literateCount + movers.literateCount, this.illiterateCount + movers.illiterateCount)
  }

  def totalCount: Int = literateCount + illiterateCount
}

class PopulationPromotionDemotion(population: RegionPopulation, random: Random = Random, maxRatio: Map[PopulationType, Double] = maxPop) {

  private case class PopulationMovement(from: Population, to: Population, count: Int)

  private val maxPopulation: Map[PopulationType, Boolean] = {
    val totalPopulation = population.pops.map(_.populationCount).sum
    Population.populationTypes.map { pt =>
      pt -> (population.pops.filter(_.populationType == pt).map(_.populationCount).sum >= totalPopulation * maxRatio(pt))
    }.toMap
  }

  private def whereToPromote(population: Population, regionPopulation: RegionPopulation): Option[Population] = {
    val possibleChanges = Population.PopulationPromotionDemotion(population.populationType)
    val chances = possibleChanges.
      filter(pc => population.populationType <= pc).
      filter(pc => regionPopulation.pop(pc, population.culture).populationCount > 0).
      filterNot(maxPopulation).
      filter(pc => regionPopulation.pop(pc, population.culture).happiness >= MaxHappinessToDemote).
      map(p => regionPopulation.pop(p, population.culture)).sortBy(-_.happiness)
    chances.headOption
  }

  def possibleRandomMovement: Set[PopulationType] = maxPopulation.filter(!_._2).keys.toSet

  private def whereToDemote(population: Population, regionPopulation: RegionPopulation): Option[Population] = {
    val possibleChanges = Population.PopulationPromotionDemotion(population.populationType)
    val chances = possibleChanges.
      filter(pc => population.populationType >= pc).
      filter(pc => regionPopulation.pop(pc, population.culture).populationCount > 0).
      filterNot(maxPopulation).
      map(p => regionPopulation.pop(p, population.culture)).sortBy(-_.happiness)
    chances.headOption
  }

  def promoteOrDemote(): Unit = {
    val movements = population.pops.flatMap { p =>

      val demotionOpt = if (p.happiness < MaxHappinessToDemote || maxPopulation(p.populationType)) {
        whereToDemote(p, population).map { target =>
          val percentage = PromotionOptionalPart
          PopulationMovement(p, target, (p.populationCount * percentage) toInt)
        }
      } else None

      val promotionOpt = if (p.happiness > MinHappinessToPromote) {
        whereToPromote(p, population).map { target =>
          val percentage = PromotionOptionalPart
          PopulationMovement(p, target, (p.populationCount * percentage) toInt)
        }
      } else None

      val randomChangePossibilities = Population.PopulationPromotionDemotion(p.populationType).filter(possibleRandomMovement.contains)
      val randomPop = if (randomChangePossibilities.nonEmpty) {
        val randomChange = randomChangePossibilities(random.nextInt(randomChangePossibilities.size))
        val randomChangeCount = p.populationCount * PromotionRequiredPart
        Some(PopulationMovement(p, population.pop(randomChange, p.culture), randomChangeCount.toInt))
      } else None


      promotionOpt ++ demotionOpt ++ randomPop
    }

    movements.foreach { movement =>
      if (movement.from.populationCount != 0) {
        val count = if (movement.count == 0) 1 else movement.count
        val move = if (movement.from.populationType > movement.to.populationType) {
          movement.from.extractIlliterateThenLiterate(count)
        } else if (movement.from.populationType < movement.to.populationType) {
          movement.from.extractLiterateThenIlliterate(count)
        } else {
          movement.from.extractRandomMovers(count)
        }
        movement.to.applyMovers(move)
      }
    }
  }
}