package mr.merc.economics

import mr.merc.economics.Population.{LifeNeeds, LuxuryNeeds, PopulationNeedsType, RegularNeeds}

object SpendingPolicy {

  def zeroSpending: SpendingPolicy = SpendingPolicy(0, 0, 0)

  sealed abstract class Spending

  object ScholarsSalary extends Spending
  object BureaucratsSalary extends Spending
  object Pensions extends Spending
  object Construction extends Spending
  object Army extends Spending
  object Reparations extends Spending

  val allSpending = List(ScholarsSalary, BureaucratsSalary, Pensions, Construction, Army)
}

case class SpendingPolicy(scholarsSalary: Double, bureaucratsSalary: Double, pensions: Double) {

  def scaleSpendingPolicy(actualMoney: Double):SpendingPolicy = {
    val sum = scholarsSalary + bureaucratsSalary + pensions

    if (sum <= actualMoney || sum == 0) this
    else {
      val m = actualMoney / sum
      SpendingPolicy(scholarsSalary * m, bureaucratsSalary * m, pensions * m)
    }
  }
}

object SpendingPolicyConfig {

  def needsSize(d: Double): Map[PopulationNeedsType, Double] = {
    require(d <= 1 && d >= 0, s"d=$d and it must be in [0, 1]")
    val third = 1d/3
    if (d <= third) {
      Map(LifeNeeds -> d * 3)
    } else if (d <= 2 * third) {
      Map(LifeNeeds -> 1d, RegularNeeds -> 3 * (d - third))
    } else {
      Map(LifeNeeds -> 1d, RegularNeeds -> 1d, LuxuryNeeds -> 3 * (d - 2 * third))
    }
  }
}

case class SpendingPolicyConfig(scholarsNeeds: Double, bureaucratsNeeds: Double, pensionsNeeds: Double, armyNeeds: Double) {

  def scholarsNeedsMap:Map[PopulationNeedsType, Double] = SpendingPolicyConfig.needsSize(scholarsNeeds)

  def bureaucratsNeedsMap:Map[PopulationNeedsType, Double] = SpendingPolicyConfig.needsSize(bureaucratsNeeds)

  def pensionsNeedsMap:Map[PopulationNeedsType, Double] = SpendingPolicyConfig.needsSize(pensionsNeeds)
}
