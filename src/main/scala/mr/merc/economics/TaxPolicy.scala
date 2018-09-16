package mr.merc.economics

import mr.merc.economics.Population.PopulationClass



object TaxPolicy {
  def zeroTaxes: TaxPolicy = TaxPolicy(CorporateTaxPolicy(0), SalaryTaxPolicy(
    Population.populationClasses.map(_ -> 0d).toMap), SalesTaxPolicy(0), TariffTax(0), TransitTax(0))

  sealed abstract class Tax
  object Corporate extends Tax
  object Salary extends Tax
  object Sales extends Tax
  object Tariff extends Tax
  object Transit extends Tax

  val allTaxes = List(Corporate, Salary, Sales, Tariff, Transit)

}

case class TaxPolicy(corporateTaxPolicy: CorporateTaxPolicy, citizensTaxPolicy: SalaryTaxPolicy,
                     salesTaxPolicy: SalesTaxPolicy, tariffTax: TariffTax, transitTax:TransitTax) {
}

case class CorporateTaxPolicy(corporateTax: Double) {
  require(corporateTax >= 0 && corporateTax <= 1.0,
    s"corporate tax must be in [0, 1] but it is $corporateTax")
}

case class SalaryTaxPolicy(salaryTax:Map[PopulationClass, Double]) {
  require(salaryTax.values.forall(s => s >= 0 && s <= 1.0),
    s"salary tax for all classes must be in [0, 1] but it is $salaryTax")
}

case class SalesTaxPolicy(salesTax: Double) {
  require(salesTax >= 0, s"sales tax must be in [0, +inf] but it is $salesTax")
}

case class TariffTax(tariffTax: Double) {
  require(tariffTax >= 0, s"tariff tax must be in [0, +inf] but it is $tariffTax")
}

case class TransitTax(transitTax: Double) {
  require(transitTax >= 0, s"transit tax must be in [0, +inf] but it is $transitTax")
}