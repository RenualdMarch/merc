package mr.merc.economics

import mr.merc.economics.Seasons.{Autumn, Spring, Summer, Winter}
import mr.merc.local.Localization

object Seasons {
  sealed trait Season
  object Winter extends Season
  object Spring extends Season
  object Summer extends Season
  object Autumn extends Season
}

object SeasonOfYear {

  def date(turn:Int):SeasonOfYear = {
    val season = turn % 4 match {
      case 0 => Winter
      case 1 | -3 => Spring
      case 2 | -2 => Summer
      case 3 | -1 => Autumn
    }
    val year = turn / 4 + 1
    SeasonOfYear(year, season)
  }
}

case class SeasonOfYear(year: Int, season:Seasons.Season) {

  def localizedString:String = season match {
    case Winter => Localization("date.winter", year)
    case Spring => Localization("date.spring", year)
    case Summer => Localization("date.summer", year)
    case Autumn => Localization("date.autumn", year)
  }

  def turn:Int = {
    year * 4 + (season match {
      case Winter => 0
      case Spring => 1
      case Summer => 2
      case Autumn => 3
    })
  }
}
