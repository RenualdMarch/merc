package mr.merc.economics

import mr.merc.economics.TaxPolicy.Income

object TaxPolicy {
  def zeroTaxes: TaxPolicy = TaxPolicy(Map())

  sealed abstract class Income
  object CorporateTax extends Income
  object LowSalaryTax extends Income
  object MiddleSalaryTax extends Income
  object UpperSalaryTax extends Income
  object SalesTax extends Income
  object TariffTax extends Income
  object TransitTax extends Income

  val allTaxes = List(CorporateTax, LowSalaryTax, MiddleSalaryTax, UpperSalaryTax, SalesTax, TariffTax, TransitTax)

}

case class TaxPolicy(private var taxes:Map[Income, Double]) {
  def apply(income: Income): Double = taxes.getOrElse(income, 0d)

  def set(income: Income, v: Double): Unit = {
    taxes += income -> v
  }
}
