package mr.merc.economics

import mr.merc.economics.Population._
import mr.merc.economics.Products._

import mr.merc.economics.MapUtil.MapWithOperations

class Population(val culture: Culture, val populationType: PopulationType, private var count: Double,
                 startingMoney: Double, private var startingLiteracy: Double) {
  require(needs.nonEmpty, s"Needs for culture $culture for type $populationType are empty!")

  private val needsFulfillmentRecordsMaxSize = 30
  private val salaryRecordsMaxSize = 30

  private var tax: SalaryTaxPolicy = _
  private var currentSalaryRecord: SalaryRecord = _

  def populationCount:Int = count.toInt

  def needs:Map[PopulationNeedsType, Map[Products.Product, Double]] = culture.needs(populationType.populationClass).
    map{ case (nt, m) => nt -> m.mapValues(_ * count * efficiency)}

  private var needsFulfillmentRecords = Vector[ProductFulfillmentRecord]()

  def needsFulfillment(n: Int):Vector[ProductFulfillmentRecord] = needsFulfillmentRecords.take(n)

  private var salaryRecords = Vector[SalaryRecord]()

  def salary(n: Int): Vector[SalaryRecord] = salaryRecords.take(n)

  private var currentMoney = startingMoney

  def moneyReserves: Double = currentMoney

  def calculateDemands(prices: Map[Product, Double]): Map[Product, Double] = {
    val lifeNeeds = buyAsMuchAsPossible(moneyReserves, prices, needs(LifeNeeds))
    val regularNeeds = buyAsMuchAsPossible(moneyReserves - moneyCost(prices, lifeNeeds), prices, needs(RegularNeeds))
    val luxNeeds = buyAsMuchAsPossible(moneyReserves - moneyCost(prices, lifeNeeds) - moneyCost(prices, regularNeeds), prices, needs(LuxuryNeeds))
    lifeNeeds |+| regularNeeds |+| luxNeeds
  }

  private def buyAsMuchAsPossible(money: Double, prices: Map[Product, Double], demands:Map[Product, Double]):Map[Product, Double] = {
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
    bought.foldLeft(0d){case(sum, (product, boughtProduct)) =>
      sum + boughtProduct * prices(product)
    }
  }

  def efficiency:Double = 1 + literacy * literacy * (MaxLiteracyEfficiencyMultiplier - 1)

  def totalPopEfficiency: Double = count * efficiency

  def literacy: Double = startingLiteracy

  def newDay(tax: SalaryTaxPolicy): Unit = {
    this.tax = tax
    salaryRecords +:= this.currentSalaryRecord
    if (salaryRecords.size > salaryRecordsMaxSize) {
      salaryRecords = salaryRecords.take(salaryRecordsMaxSize)
    }

    this.currentSalaryRecord = SalaryRecord(populationCount, 0, 0, 0)
  }

  private case class DemandInfo(product: Product, count: Double, price: Double)

  private var alreadyReceivedProducts: List[FulfilledDemandRequest] = Nil

  // returns spent money
  def buyDemandedProducts(requests: List[FulfilledDemandRequest]): Double = {
    assert(requests.forall(r => r.request.asInstanceOf[PopulationDemandRequest].pop == this))

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

    val productFulfillment = new ProductFulfillmentRecord(needs, receivedProductsMap)
    needsFulfillmentRecords +:= productFulfillment
    if (needsFulfillmentRecords.size > needsFulfillmentRecordsMaxSize) {
      needsFulfillmentRecords = needsFulfillmentRecords.takeRight(needsFulfillmentRecordsMaxSize)
    }
  }

  // TODO add info about salary sources
  def receiveSalary(salary: Double): Unit = {
    val taxPart = tax.salaryTax(this.populationType.populationClass) * salary
    currentMoney = salary + currentMoney - taxPart
    val r = this.currentSalaryRecord
    this.currentSalaryRecord = this.currentSalaryRecord.copy(populationCount, r.receivedMoney + salary, currentMoney, r.taxes + taxPart)
  }

  def payTaxes(): Double = this.currentSalaryRecord.taxes

  override def toString: String = s"$culture $populationType"
}

object Population extends EconomicConfig {
  val MaxLiteracyEfficiencyMultiplier = 10

  sealed abstract class PopulationNeedsType(val needImportance:Int)
  case object LifeNeeds extends PopulationNeedsType(5)
  case object RegularNeeds extends PopulationNeedsType(3)
  case object LuxuryNeeds extends PopulationNeedsType(1)

  sealed trait PopulationClass

  case object Lower extends PopulationClass
  case object Middle extends PopulationClass
  case object Upper extends PopulationClass

  def populationClasses:List[PopulationClass] = List(Lower, Middle, Upper)

  sealed abstract class PopulationType(val populationClass: PopulationClass)

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

  def populationTypes:List[PopulationType] =
    List(Craftsmen, Farmers, Labourers, Bureaucrats,
      Scholars, Mages, Capitalists, Aristocrats)

  def populationTypesByClass:Map[PopulationClass, List[PopulationType]] =
    populationTypes.groupBy(_.populationClass)

  type ProductBracket = Map[Products.Product, Double]

  type PopulationNeeds = Map[PopulationClass, Map[PopulationNeedsType, ProductBracket]]

  private implicit class ScaleToConfig[T](map:Map[T, Double]) {
    def scale(name: String):Map[T, Double] = {
      import com.github.andr83.scalaconfig._
      val s = config.asUnsafe[Double](name)
      map.scaleToSum(s)
    }
  }

  private def need(scalingConfigName: String, needs: Tuple2[Products.Product, Double]*): Map[Products.Product, Double] = {
    needs.toMap.scale(s"population.needs.$scalingConfigName")
  }

  private def defaultHumanNeeds(culture: Culture):PopulationNeeds = Map(
    Lower -> Map(
    LifeNeeds -> need("lower.life", Grain -> 3d, Fish -> 1d, Fruit -> 1d, Cattle -> 1d),
    RegularNeeds -> need("lower.regular", Tea -> 1, Clothes -> 1, Liquor -> 2, Furniture -> 1, Coal -> 1,
      Amulet -> 1, Medicine -> 1, Lumber -> 1, Ritual(culture) -> 1),
    LuxuryNeeds -> need("lower.luxury", Furniture -> 2, Clothes -> 3, Paper -> 1, Coffee -> 1, Liquor -> 2,
      Medicine -> 1, Amulet -> 1, Weapons -> 1, Coal -> 1, Ritual(culture) -> 2)
  ),
    Middle -> Map(
    LifeNeeds -> need("middle.life", Grain -> 4, Fish -> 2, Fruit -> 2, Cattle -> 2),
    RegularNeeds -> need("middle.regular", Tea -> 2, Clothes -> 3, Liquor -> 2, Furniture -> 2, Coal -> 2, Ritual(culture) -> 1,
      Glass -> 1, Wine -> 2, Amulet -> 2, Medicine -> 2, Paper -> 3, Weapons -> 1, Cement -> 1),
    LuxuryNeeds -> need("middle.luxury", Clothes -> 3, Wine -> 3, Amulet -> 3, Medicine -> 3,
      Furniture -> 3, Opium -> 3, Paper -> 3, Weapons -> 2, Cement -> 2, Ritual(culture) -> 2)
  ),
    Upper -> Map(
      LifeNeeds -> need("upper.life", Grain -> 6, Fish -> 3, Fruit -> 3, Cattle -> 3),
      RegularNeeds -> need("upper.regular", Tea -> 5, Clothes -> 5, Liquor -> 5, Furniture -> 5, Coal -> 10, Cement -> 5,
        Glass -> 5, Wine -> 10, Amulet -> 5, Medicine -> 5, Paper -> 10, Weapons -> 5, Opium -> 5, Ritual(culture) -> 1),
      LuxuryNeeds -> need("upper.luxury", Clothes -> 5, Furniture -> 5, Coal -> 5, Paper -> 5, Amulet -> 5,
        Medicine -> 5, Weapons -> 5, Cement -> 5, Opium -> 5, Ritual(culture) -> 2)
    )
  )


  // removed sealed for test purposes only
  abstract class Race()
  case object Humans extends Race()

  //
  // DISABLED RACES
  //
  case object Elves extends Race()
  case object Dwarfs extends Race()
  case object Orcs extends Race()
  case object Saurians extends Race()
  case object Drakes extends Race()
  case object Undead extends Race()
  case object Demons extends Race()

  def races = List(Humans) //, Elves, Dwarfs, Orcs, Saurians, Drakes, Undead, Demons)

  // removed sealed for test purposes only
  abstract class Culture(val stateNameKey:String, val race:Race) {
    def needs: PopulationNeeds = defaultHumanNeeds(this)
  }
  case object LatinHuman extends Culture("state.empire", Humans)
  case object WesternHuman extends Culture("state.kingdom", Humans)

  //
  // DISABLED CULTURES
  //
  case object HighElf extends Culture("state.federation", Elves)
  case object DarkElf extends Culture("state.confederation", Elves)
  case object BarbarianOrc extends Culture("state.horde", Orcs)
  case object RockDwarf extends Culture("state.clan", Dwarfs)
  case object GreenSaurian extends Culture("state.syndicate", Saurians)
  case object OldDrakes extends Culture("state.dominion", Drakes)
  case object Forsaken extends Culture("state.collective", Undead)
  case object RedDemons extends Culture("state.legion", Demons)

  // good words: alliance, protectorate, tribe, army

  def cultures = List(LatinHuman, WesternHuman) //, HighElf, DarkElf, BarbarianOrc, RockDwarf, GreenSaurian, OldDrakes, Forsaken, RedDemons)

  class ProductFulfillmentRecord(needs: Map[PopulationNeedsType, Map[Products.Product, Double]], products: Map[Product, Double]) {

    val needsFulfillmentInfo:Map[PopulationNeedsType, List[ProductFulfillment]] = {

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

    val needsFulfillment: Map[PopulationNeedsType, Double] = needsFulfillmentInfo.transform{
      case (_, list) =>
        val totalDemanded = list.map(_.demanded).sum
        val totalReceived = list.map(_.received).sum
        totalReceived / totalDemanded
    }

    private def calculateProductFulfillment(needs: Map[Product, Double], afterFulfillment:Map[Product, Double]):List[ProductFulfillment] = {
      needs.toList.map { case (product, productNeed) =>
        val remainedProduct = afterFulfillment.getOrElse(product, -productNeed)
        if (remainedProduct > 0) ProductFulfillment(product, productNeed, productNeed)
        else {
          val received = productNeed + remainedProduct
          ProductFulfillment(product, productNeed, received)
        }
      }
    }

    private def removeZeroesAndNegative(map: Map[Product, Double]): Map[Product, Double] = map.filter(_._2 > 0)

  }

  case class ProductFulfillment(product:Product, demanded: Double, received: Double)
  // TODO add money source
  case class SalaryRecord(populationCount: Int, receivedMoney: Double, totalMoney: Double, taxes: Double)
}

