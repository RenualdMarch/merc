package mr.merc.economics

import mr.merc.economics.Population._
import mr.merc.economics.Products._
import cats.implicits._

class Population(val culture: Culture, val populationType: PopulationType, count: Int, startingMoney: Double) {
  require(needs.nonEmpty, s"Needs for culture $culture for type $populationType are empty!")

  private val needsFulfillmentRecordsMaxSize = 30
  private val salaryRecordsMaxSize = 30

  def needs:Map[PopulationNeedsType, Map[Products.Product, Double]] = culture.race.needs(populationType.populationClass).
    map{ case (nt, m) => nt -> m.mapValues(_ * count)}

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

  private case class DemandInfo(product: Product, count: Double, price: Double)

  // returns spent money
  def receiveProductsAndPayChecks(products: Map[Product, FulfilledDemandRequest]): Double = {
    val boughtProducts = products.map{ case (p, f) => p -> f.bought}
    fulfillNeeds(boughtProducts)

    val prices = products.map{ case (p, f) => p -> f.price}

    val spentMoney = moneyCost(prices, boughtProducts)
    currentMoney = currentMoney - spentMoney
    spentMoney
  }

  private def fulfillNeeds(products:Map[Product, Double]): Unit = {
    val productFulfillment = new ProductFulfillmentRecord(needs, products)
    needsFulfillmentRecords +:= productFulfillment
    if (needsFulfillmentRecords.size > needsFulfillmentRecordsMaxSize) {
      needsFulfillmentRecords = needsFulfillmentRecords.take(needsFulfillmentRecordsMaxSize)
    }
  }

  // TODO add info about salary sources
  def receiveSalary(salary: Double): Unit = {
    val salaryRecord = SalaryRecord(count, salary)
    currentMoney = salary + currentMoney
    salaryRecords +:= salaryRecord
    if (salaryRecords.size > salaryRecordsMaxSize) {
      salaryRecords = salaryRecords.take(salaryRecordsMaxSize)
    }
  }
}

object Population {
  sealed abstract class PopulationNeedsType(val needImportance:Int)
  case object LifeNeeds extends PopulationNeedsType(5)
  case object RegularNeeds extends PopulationNeedsType(3)
  case object LuxuryNeeds extends PopulationNeedsType(1)

  sealed trait PopulationClass

  case object Lower extends PopulationClass
  case object Middle extends PopulationClass
  case object Upper extends PopulationClass

  sealed abstract class PopulationType(val populationClass: PopulationClass)

  //// craftsmen work in factories
  case object Craftsmen extends PopulationType(Lower)

  // farmers gather resources that are plants:
  case object Farmers extends PopulationType(Lower)

  // labourers gather resources in mines
  case object Labourers extends PopulationType(Lower)

  // can replace Farmers, Labourers or Craftsmen and work for food only
  case object Slaves extends PopulationType(Lower)

  // Bureaucrats work with papers, they are administrators
  case object Bureaucrats extends PopulationType(Middle)

  // scholar teach people literacy and can invent
  case object Scholars extends PopulationType(Middle)

  // mages produce medicine and amulets
  case object Mages extends PopulationType(Middle)

  // traders take percent from market transactions in their region
  case object Traders extends PopulationType(Middle)

  // capitalists own factories and receive profits from them
  case object Capitalists extends PopulationType(Upper)

  // aristocrats own land and receive from resource gathering.
  // They are also best soldiers
  case object Aristocrats extends PopulationType(Upper)

  def populationTypes:List[PopulationType] =
    List(Craftsmen, Farmers, Labourers, Slaves, Bureaucrats,
      Scholars, Mages, Capitalists, Aristocrats)

  def populationTypesByClass:Map[PopulationClass, List[PopulationType]] =
    populationTypes.groupBy(_.populationClass)

  type PopulationNeeds = Map[PopulationClass, Map[PopulationNeedsType, Map[Products.Product, Double]]]

  private val HumanNeeds:PopulationNeeds = Map(
    Lower -> Map(
    LifeNeeds -> Map(Grain -> 3, Fish -> 1, Fruit -> 1, Cattle -> 1),
    RegularNeeds -> Map(Tea -> 1, Clothes -> 1, Liquor -> 2, Furniture -> 1, Coal -> 1,
      Amulet -> 1, Medicine -> 1, Lumber -> 1),
    LuxuryNeeds -> Map(Furniture -> 2, Clothes -> 3, Paper -> 1, Coffee -> 1, Liquor -> 2,
      Medicine -> 1, Amulet -> 1, Weapons -> 1, Coal -> 1)
  ),
    Middle -> Map(
    LifeNeeds -> Map(Grain -> 4, Fish -> 2, Fruit -> 2, Cattle -> 2),
    RegularNeeds -> Map(Tea -> 2, Clothes -> 3, Liquor -> 2, Furniture -> 2, Coal -> 2,
      Glass -> 1, Wine -> 2, Amulet -> 2, Medicine -> 2, Paper -> 3, Weapons -> 1, Cement -> 1),
    LuxuryNeeds -> Map(Clothes -> 3, Wine -> 3, Amulet -> 3, Medicine -> 3,
      Furniture -> 3, Opium -> 3, Paper -> 3, Weapons -> 2, Cement -> 2)
  ),
    Upper -> Map(
      LifeNeeds -> Map(Grain -> 6, Fish -> 3, Fruit -> 3, Cattle -> 3),
      RegularNeeds -> Map(Tea -> 5, Clothes -> 5, Liquor -> 5, Furniture -> 5, Coal -> 10, Cement -> 5,
        Glass -> 5, Wine -> 10, Amulet -> 5, Medicine -> 5, Paper -> 10, Weapons -> 5, Opium -> 5),
      LuxuryNeeds -> Map(Clothes -> 5, Furniture -> 5, Coal -> 5, Paper -> 5, Amulet -> 5,
        Medicine -> 5, Weapons -> 5, Cement -> 5, Opium -> 5)
    )
  )


  // removed sealed for test purposes only
  abstract class Race(val needs: PopulationNeeds)
  case object Humans extends Race(HumanNeeds)

  //
  // DISABLED RACES
  //
  case object Elves extends Race(Map())
  case object Dwarfs extends Race(Map())
  case object Orcs extends Race(Map())
  case object Saurians extends Race(Map())
  case object Drakes extends Race(Map())
  case object Undead extends Race(Map())
  case object Demons extends Race(Map())

  def races = List(Humans) //, Elves, Dwarfs, Orcs, Saurians, Drakes, Undead, Demons)

  // removed sealed for test purposes only
  abstract class Culture(val stateNameKey:String, val race:Race)
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
      import EconomicUtil.subtractMapFromMap

      val lifeNeeds = needs(LifeNeeds)
      val afterLifeNeeds = subtractMapFromMap(products, lifeNeeds)
      val lifeNeedsFulfillment = calculateProductFulfillment(lifeNeeds, afterLifeNeeds)
      val beforeRegularNeeds = removeZeroesAndNegative(afterLifeNeeds)
      val regularNeeds = needs(RegularNeeds)
      val afterRegularNeeds = subtractMapFromMap(beforeRegularNeeds, regularNeeds)
      val regularNeedsFulfillment = calculateProductFulfillment(regularNeeds, afterRegularNeeds)
      val beforeLuxNeeds = removeZeroesAndNegative(afterRegularNeeds)
      val luxuryNeeds = needs(LuxuryNeeds)
      val afterLuxNeeds = subtractMapFromMap(beforeLuxNeeds, luxuryNeeds)
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
  case class SalaryRecord(count: Int, totalMoney: Double)
}

