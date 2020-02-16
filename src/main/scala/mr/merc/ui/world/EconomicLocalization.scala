package mr.merc.ui.world

import mr.merc.army.{Warrior, WarriorCompetence, WarriorType}
import mr.merc.army.WarriorCompetence.{Militia, Professional}
import mr.merc.economics.Products.Ritual
import mr.merc.economics._
import mr.merc.local.Localization
import mr.merc.politics.Province

object EconomicLocalization {

  def title(e: Enterprise): String = e match {
    case _: IndustrialFactory => Localization("factory")
    case _: Farm => Localization("farm")
    case _: Mine => Localization("mine")
    case _: Church => Localization("church")
    case _: MagicGuild => Localization("magesGuild")
  }

  def localizeEnterprise(e: Enterprise, p: Province): String = {
    Localization("enterprise.title",
      Localization(e.product.name), title(e), p.name)
  }

  def localizePopulation(p: Population, province: Province): String = Localization("population.title",
    p.culture.race.name, p.culture.name, p.populationType.name, province.name)

  def localizePopulation(p: Population): String = Localization("population.titleSmall",
    p.culture.race.name, p.culture.name, p.populationType.name)

  def localizeProject(project:BusinessProject):String = {
    project match {
      case p:PopulationExpandFactory => Localization("project.private.expandFactory", Localization(p.factory.product.name))
      case p:StateExpandFactory => Localization("project.state.expandFactory", Localization(p.factory.product.name))
      case p:PopulationBuildFactory => Localization("project.private.buildFactory", Localization(p.product.name))
      case p:StateBuildFactory => Localization("project.state.buildFactory", Localization(p.product.name))
    }
  }

  def localizeProjectShort(project: BusinessProject):String = {
    project match {
      case p:PopulationExpandFactory  => Localization("project.expandFactory", Localization(p.factory.product.name))
      case p:StateExpandFactory => Localization("project.expandFactory", Localization(p.factory.product.name))
      case p:PopulationBuildFactory => Localization("project.buildFactory", Localization(p.product.name))
      case p:StateBuildFactory => Localization("project.buildFactory", Localization(p.product.name))
      case p:StateRecruitWarrior => Localization("project.recruitWarrior", localizeWarriorTypeInOneLine(p.competence, p.warriorType))
    }
  }

  def localizeProduct(p: Products.Product): String = p match {
    case r:Ritual => Localization(s"product.ritual", Localization(r.culture.name))
    case _ => Localization("product." + p.name.toLowerCase)
  }

  def localizeCulture(c:Culture): String = Localization(c.cultureNameKey)

  def localizeProductsBucket(products:Map[Products.Product, Double]): String = {
    products.map{ case (p, amount) =>
        s"${localizeProduct(p)}:${DoubleFormatter().format(amount)}"
    }.mkString(", ")
  }

  def localizeWarriorType(warrior: Warrior): String = localizeWarriorTypeInOneLine(warrior.competence, warrior.warriorType)

  def localizeWarriorCompetence(competence: WarriorCompetence): String = competence match {
    case Militia => Localization("army.militia")
    case Professional => Localization("army.professional")
  }

  def localizeWarriorTypeInOneLine(competence: WarriorCompetence, warriorType: WarriorType): String = {
    val competenceString = localizeWarriorCompetence(competence)
    val warriorTypeString = Localization(warriorType.name)
    s"$competenceString $warriorTypeString"
  }
}
