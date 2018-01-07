package mr.merc.economics

class Taxes {

}

case class FactoryTaxPolicy(corporateTax: Double) {
  require(corporateTax >= 0 & corporateTax <= 1.0,
    s"corporate tax must be in [0, 1] but it is $corporateTax")
}

case class CitizensTaxPolicy()