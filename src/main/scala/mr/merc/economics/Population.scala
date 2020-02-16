package mr.merc.economics

import mr.merc.economics.Population._
import mr.merc.economics.Products._
import mr.merc.economics.MapUtil.FloatOperations._
import mr.merc.economics.TaxPolicy.{LowSalaryTax, MiddleSalaryTax, UpperSalaryTax}
import mr.merc.politics.{PoliticalViews, State}
import WorldConstants.Population._
import mr.merc.economics.PopulationMigrationInsideProvince.PopulationProvinceMovement

import scala.util.Random

class Population(val culture: Culture, val populationType: PopulationType, private var count: Double,
                 startingMoney: Double, private val startingliterateCount: Int, val politicalViews: PoliticalViews) {
  require(needs.nonEmpty, s"Needs for culture $culture for type $populationType are empty!")

  private var literatePeople = startingliterateCount.toDouble

  def literateCount: Int = literatePeople.toInt

  private val salaryRecordsMaxSize = 30

  private var tax: Double = 0d
  private var currentPopulationDayRecord: PopulationDayRecord = PopulationDayRecord(populationCount, 0, 0, 0, 0,
    new ProductFulfillmentRecord(Map(), needs, Map()), Nil)

  def currentDayRecord: PopulationDayRecord = currentPopulationDayRecord

  def populationCount: Int = count.toInt

  def needs: Map[PopulationNeedsType, Map[Products.Product, Double]] = {
    val illit = culture.needs.illiterateNeeds(populationType.populationClass)
    val lit = culture.needs.literateNeeds(populationType.populationClass)

    val total = (lit |**| literacy) |++| (illit |**| (1 - literacy))

    total.map { case (nt, m) => nt -> m.mapValues(_ * count * efficiency) }
  }

  def needsFulfillment(i: Int):Vector[ProductFulfillmentRecord] = populationDayRecords.takeRight(i).map(_.productFulfillment)

  // last $HappinessDayCount turns
  def consumptionHappiness: Double = {
    val n = List(currentDayRecord.productFulfillment)//needsFulfillment(HappinessDayCount)
    val sum = HappinessLifeNeedsMultiplier + HappinessRegularNeedsMultiplier + HappinessLuxuryNeedsMultiplier

    if (n.isEmpty) {
      EmptyPopConsumptionHappiness
    } else {
      n.map { f =>
        val needs = f.needsFulfillment(LifeNeeds) * HappinessLifeNeedsMultiplier +
          f.needsFulfillment(RegularNeeds) * HappinessRegularNeedsMultiplier +
          f.needsFulfillment(LuxuryNeeds) * HappinessLuxuryNeedsMultiplier
        needs / sum
      }.sum / n.size
    }
  }

  def politicalHappiness(state:State):Double = {
    val popPositions = politicalViews.currentViews(this.literacy).pointsOfView
    val partyPos = state.rulingParty.politicalPosition
    val notAgreeingValue = popPositions.map { case (popPos, v) =>
      partyPos.diffWithPosition(popPos) * v
    }.sum * PoliticalHappinessDisagreementMultiplier

    val noCulturePenalty = if (notAgreeingValue > 1) 0 else 1 - notAgreeingValue
    if (state.primeCulture == culture) noCulturePenalty
    else if (noCulturePenalty > DifferentCulturePoliticalHappinessPenalty) {
      noCulturePenalty - DifferentCulturePoliticalHappinessPenalty
    } else 0
  }

  def growthRate:Double = {
    populationDayRecords.lastOption.map { last =>
      val f = last.productFulfillment.needsFulfillment
      BasePopGrowth + f(LifeNeeds) * GrowthRatePerLifeNeed + f(RegularNeeds) * GrowthRatePerRegularNeed
    }.getOrElse(BasePopGrowth)
  }

  def grow(): Unit = {
    count *= (1 + growthRate)
  }

  def extractLiterateMovers(count: Int): PopulationMovers = {
    val change = if (count < literateCount) count else literateCount
    literatePeople -= change
    this.count -= change
    PopulationMovers(change, 0)
  }

  def extractIlliterateMovers(count: Int): PopulationMovers = {
    val change = if (count < populationCount - literateCount) count else populationCount - literateCount
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

  def addProvinceMovement(movement:PopulationProvinceMovement): Unit = {
    this.movements = movement :: this.movements
  }

  private var populationDayRecords = Vector[PopulationDayRecord]()

  def salary: Vector[PopulationDayRecord] = populationDayRecords

  private var currentPrices: Map[Product, Double] = Map()

  private var currentMoney = startingMoney

  private var movements:List[PopulationProvinceMovement] = Nil

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

  def literacy: Double = if (populationCount == 0) 0 else literatePeople / count

  private val taxPolicy = this.populationType.populationClass match {
    case Lower => LowSalaryTax
    case Middle => MiddleSalaryTax
    case Upper => UpperSalaryTax
  }

  def newDay(stateTaxPolicy: TaxPolicy, bureaucratsPercentage: Double): Unit = {
    this.tax = stateTaxPolicy.tax(taxPolicy, bureaucratsPercentage)
    this.currentPopulationDayRecord = PopulationDayRecord(populationCount, 0, 0, 0, 0,
      new ProductFulfillmentRecord(Map(), needs, Map()), this.movements)
    this.movements = Nil
    this.currentPrices = Map()
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

  def learnLiteracy(students: Double): Unit = {
    if (count - literatePeople > students) {
      literatePeople += students
    } else {
      literatePeople = count
    }
  }

  private case class DemandInfo(product: Product, count: Double, price: Double)

  private var alreadyReceivedProducts: List[FulfilledDemandRequest] = Nil

  def buyDemandedProducts(requests: List[FulfilledDemandRequest]): Unit = {
    assert(requests.forall(r => r.request.asInstanceOf[PopulationDemandRequest].pop == this))

    currentPrices ++= requests.map { f =>
      f.request.product -> f.price
    }.toMap

    alreadyReceivedProducts ++= requests

    requests.foreach { request =>
      request.currentSpentMoney += request.spentMoney
      currentMoney -= request.spentMoney
    }
  }

  def fulfillNeedsUsingAlreadyReceivedProducts(): Unit = {
    val receivedProductsMap = alreadyReceivedProducts.map { r =>
      Map(r.request.product -> r.bought)
    }.fold(Map())(_ |+| _)

    val productFulfillment = new ProductFulfillmentRecord(currentPrices, needs, receivedProductsMap)
    this.currentPopulationDayRecord = currentPopulationDayRecord.copy(productFulfillment = productFulfillment)
    alreadyReceivedProducts = Nil
  }

  // TODO add info about salary sources
  def receiveSalary(salary: Double, payTax: Boolean = true): Unit = {
    val taxPart = if (payTax) tax * salary else 0

    currentMoney = salary + currentMoney
    val r = this.currentPopulationDayRecord
    this.currentPopulationDayRecord = this.currentPopulationDayRecord.copy(populationCount, r.receivedMoney + salary, currentMoney - taxPart, r.taxes + taxPart)
  }

  def payTaxes(region:EconomicRegion): Unit = {
    this.currentMoney -= this.currentPopulationDayRecord.taxes
    val td = TaxData(taxPolicy, this.currentPopulationDayRecord.receivedMoney, this.currentPopulationDayRecord.taxes)
    region.owner.budget.receiveTaxes(td)
  }


  override def toString: String = s"$culture $populationType"
}

object Population {

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

  val PopulationMigration: Map[PopulationType, List[PopulationType]] = populationTypes.map{pt =>
    pt -> populationTypesByClass(pt.populationClass).filterNot(_ == pt)
  }.toMap

  def populationTypes: List[PopulationType] =
    List(Craftsmen, Farmers, Labourers, Bureaucrats, Clergy,
      Scholars, Traders, Mages, Capitalists, Aristocrats)

  def populationTypesByClass: Map[PopulationClass, List[PopulationType]] =
    populationTypes.groupBy(_.populationClass)

  type ProductBracket = Map[Products.Product, Double]

  type CornerPopulationNeeds = Map[PopulationClass, Map[PopulationNeedsType, ProductBracket]]

  case class PopulationNeeds(illiterateNeeds: CornerPopulationNeeds, literateNeeds: CornerPopulationNeeds)

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
                                 investments: Double, productFulfillment: ProductFulfillmentRecord, provinceMovements:List[PopulationProvinceMovement])
}

case class PopulationMovers(literateCount: Int, illiterateCount: Int) {
  def +(movers: PopulationMovers): PopulationMovers = {
    PopulationMovers(this.literateCount + movers.literateCount, this.illiterateCount + movers.illiterateCount)
  }

  def totalCount: Int = literateCount + illiterateCount
}

object PopulationMigrationInsideProvince {
  case class PopulationProvinceMovement(from: Population, to: Population, count: Int)
}

class PopulationMigrationInsideProvince(population: RegionPopulation, random: Random = Random) {

  import PopulationMigrationInsideProvince._

  private def insideProvinceMigrationPercentage(fromConsumption:Double, toConsumption: Double): Double = {
    val difference = toConsumption - fromConsumption
    if (difference > 0)
      difference * MaxProvinceMigrationPart
    else 0
  }

  private def whereToMigrateInsideProvince(population: Population, regionPopulation: RegionPopulation): Option[PopulationProvinceMovement] = {
    if (population.consumptionHappiness < ConsumptionHappinessToMigrateInsideProvince && Population.PopulationMigration(population.populationType).nonEmpty) {
      val pt = Population.PopulationMigration(population.populationType).maxBy(p => regionPopulation.consumptionHappiness(p))
      val targetPop = regionPopulation.pop(pt, population.culture)
      val perc = insideProvinceMigrationPercentage(population.consumptionHappiness, targetPop.consumptionHappiness)
      if (perc > 0.001) {
        Some(PopulationProvinceMovement(population, targetPop, (population.populationCount * perc) toInt))
      } else None
    } else None
  }

  def migrateInsideProvince(): Unit = {
    val movements = population.pops.flatMap { p =>
      whereToMigrateInsideProvince(p, population)
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
        movement.from.addProvinceMovement(movement)
        movement.to.addProvinceMovement(movement)
      }
    }
  }
}