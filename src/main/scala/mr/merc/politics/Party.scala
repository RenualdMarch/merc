package mr.merc.politics

import mr.merc.politics.Economy._
import mr.merc.politics.ForeignPolicy._
import mr.merc.politics.Migration._
import mr.merc.politics.Regime._
import mr.merc.politics.SocialPolicy._
import mr.merc.politics.VotersPolicy._
import scalafx.scene.paint.Color

case class Party(name: String,
                 color: Color,
                 migration: IssuePosition.MigrationPosition,
                 regime: IssuePosition.RegimePosition,
                 foreignPolicy: IssuePosition.ForeignPolicyPosition,
                 economy: IssuePosition.EconomyPosition,
                 socialPolicy: IssuePosition.SocialPolicyPosition,
                 votersPolicy: IssuePosition.VotersPolictyPosition) {
}


object Party {
  val absolute = Party("Monarchistic", Color.White, ClosedBorders, Absolute, Expansionism, StateEconomy, NoSocialSecurity, NoVoting)

  val benevolent = Party("Enlightened Monarchistic", Color.LightGray, OpenBorders, Absolute, Expansionism, StateEconomy, LifeNeedsSocialSecurity, NoVoting)

  val magocratic = Party("Magocratic", Color.Brown, ClosedBorders, Constitutional, Expansionism, StateEconomy, NoSocialSecurity, MagesOnly)

  val theocratic = Party("Theocratic", Color.Purple, ClosedBorders, Constitutional, Expansionism, StateEconomy, NoSocialSecurity, ClericsOnly)

  val aristocratic = Party("Aristocratic", Color.LightBlue, ClosedBorders, Constitutional, Expansionism, StateEconomy, NoSocialSecurity, PrimaryUpperClass)

  val capitalistic = Party("Capitalistic", Color.LightYellow, OpenBorders, Constitutional, Expansionism, Interventionism, NoSocialSecurity, PrimaryUpperClass)

  val manufactorers = Party("Manufactorers", Color.LightGoldrenrodYellow, OpenBorders, Constitutional, Expansionism, Interventionism, NoSocialSecurity, PrimaryUpperAndMiddleClass)

  val responsibleManufactorers = Party("Responsible Manufactorers", Color.YellowGreen, OpenBorders, Constitutional, Expansionism, Interventionism, LifeNeedsSocialSecurity, PrimaryUpperAndMiddleClass)

  val paternalistic = Party("Paternalistic", Color.IndianRed, OpenBorders, Constitutional, Expansionism, StateEconomy, LifeNeedsSocialSecurity, PrimaryUpperAndMiddleClass)

  val oligarchic = Party("Oligarchic", Color.CornflowerBlue, OpenBorders, Constitutional, Expansionism, StateEconomy, NoSocialSecurity, PrimaryUpperAndMiddleClass)

  val conservative = Party("Conservative", Color.Blue, OpenBorders, Democracy, Expansionism, FreeMarket, LifeNeedsSocialSecurity, Everyone)

  val patriot = Party("Patriotic", Color.DarkBlue, OpenBorders, Democracy, Expansionism, Interventionism, LifeNeedsSocialSecurity, Everyone)

  val liberal = Party("Liberal", Color.Yellow, OpenBorders, Democracy, Pacifism, FreeMarket, LifeNeedsSocialSecurity, Everyone)

  // dark yellow
  val libertarian = Party("Libertarian", Color.rgb(153,153,0), OpenBorders, Democracy, Pacifism, FreeMarket, NoSocialSecurity, Everyone)

  val socialist = Party("Socialist", Color.Red, OpenBorders, Democracy, Pacifism, StateEconomy, RegularNeedsSocialSecurity, Everyone)

  val socialDemocratic = Party("Social Democratic", Color.Pink, OpenBorders, Democracy, Pacifism, Interventionism, RegularNeedsSocialSecurity, Everyone)

  val allParties:List[Party] = List(absolute, benevolent, magocratic, theocratic, aristocratic, capitalistic, manufactorers, responsibleManufactorers,
    paternalistic, oligarchic, conservative, liberal, libertarian, socialist, socialDemocratic, patriot)
}