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
                 votersPolicy: IssuePosition.VotersPolicyPosition) {

  def politicalPosition:PoliticalPosition = {
    PoliticalPosition(migration, regime, foreignPolicy, economy, socialPolicy, votersPolicy)
  }

}


object Party {
  val absolute = Party("party.monarchistic", Color.White, ClosedBorders, Absolute, Expansionism, StateEconomy, NoSocialSecurity, NoVoting)

  val benevolent = Party("party.enlightenedMonarchistic", Color.LightGray, OpenBorders, Absolute, Pacifism, FreeMarket, LifeNeedsSocialSecurity, NoVoting)

  val magocratic = Party("party.magocratic", Color.Brown, ClosedBorders, Constitutional, Expansionism, StateEconomy, NoSocialSecurity, MagesOnly)

  val theocratic = Party("party.theocratic", Color.Purple, ClosedBorders, Constitutional, Expansionism, StateEconomy, NoSocialSecurity, ClericsOnly)

  val aristocratic = Party("party.aristocratic", Color.LightBlue, ClosedBorders, Constitutional, Expansionism, StateEconomy, NoSocialSecurity, PrimaryUpperClass)

  val capitalistic = Party("party.capitalistic", Color.LightYellow, OpenBorders, Constitutional, Expansionism, FreeMarket, NoSocialSecurity, PrimaryUpperClass)

  val manufactorers = Party("party.manufactorers", Color.LightGoldrenrodYellow, OpenBorders, Constitutional, Expansionism, FreeMarket, NoSocialSecurity, PrimaryUpperAndMiddleClass)

  val responsibleManufactorers = Party("party.responsibleManufactorers", Color.YellowGreen, OpenBorders, Constitutional, Expansionism, FreeMarket, LifeNeedsSocialSecurity, PrimaryUpperAndMiddleClass)

  val paternalistic = Party("party.paternalistic", Color.IndianRed, OpenBorders, Constitutional, Expansionism, StateEconomy, LifeNeedsSocialSecurity, PrimaryUpperAndMiddleClass)

  val oligarchic = Party("party.oligarchic", Color.CornflowerBlue, OpenBorders, Constitutional, Expansionism, StateEconomy, NoSocialSecurity, PrimaryUpperAndMiddleClass)

  val conservative = Party("party.conservative", Color.Blue, OpenBorders, Democracy, Expansionism, FreeMarket, LifeNeedsSocialSecurity, Everyone)

  val patriot = Party("party.patriotic", Color.DarkBlue, OpenBorders, Democracy, Expansionism, StateEconomy, LifeNeedsSocialSecurity, Everyone)

  val liberal = Party("party.liberal", Color.Yellow, OpenBorders, Democracy, Pacifism, FreeMarket, LifeNeedsSocialSecurity, Everyone)

  // dark yellow
  val libertarian = Party("party.libertarian", Color.rgb(153,153,0), OpenBorders, Democracy, Pacifism, FreeMarket, NoSocialSecurity, Everyone)

  val socialist = Party("party.socialist", Color.Red, OpenBorders, Democracy, Pacifism, StateEconomy, RegularNeedsSocialSecurity, Everyone)

  val socialDemocratic = Party("party.socialDemocratic", Color.Pink, OpenBorders, Democracy, Pacifism, FreeMarket, RegularNeedsSocialSecurity, Everyone)

  val allParties:List[Party] = List(absolute, benevolent, magocratic, theocratic, aristocratic, capitalistic, manufactorers, responsibleManufactorers,
    paternalistic, oligarchic, conservative, liberal, libertarian, socialist, socialDemocratic, patriot)
}