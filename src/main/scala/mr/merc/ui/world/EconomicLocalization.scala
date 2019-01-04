package mr.merc.ui.world

import mr.merc.economics._
import mr.merc.local.Localization
import mr.merc.politics.Province

object EconomicLocalization {

  def title(e: Enterprise): String = e match {
    case _: IndustrialFactory => Localization("factory")
    case _: Farm => Localization("farm")
    case _: Mine => Localization("mine")
    case _: Church => Localization("church")
    case _: MagicGuildEnterprise => Localization("magesGuild")
  }

  def localizeEnterprise(e: Enterprise, p: Province): String = {
    Localization("enterprise.title",
      Localization(e.product.name), title(e), p.name)
  }

  def localizePopulation(p: Population, province: Province): String = Localization("population.title",
    p.culture.race.name, p.culture.name, p.populationType.name, province.name)

}
